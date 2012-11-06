#include "stdafx.h"
#include "VSVFL.h"
#include "NAND.h"

#include <boost/format.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>

using namespace boost;
using namespace std;
using phoenix::arg_names::arg1;

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
void VSVFLContext::read_from(ByteBuffer const& b)
{
    _empty = false;

    usn_inc   = b.get_uint4_le();
    usn_dec   = b.get_uint4_le();
    ftl_type  = b.get_uint4_le();
    usn_block = b.get_uint2_le();
    usn_page  = b.get_uint2_le();
    active_context_block = b.get_uint2_le();
    write_failure_count  = b.get_uint2_le();
    bad_block_count      = b.get_uint2_le();

    for (int i=0; i<4; i++) 
        replaced_block_count[i] = b.get_uint1();

    num_reserved_blocks   = b.get_uint2_le();
    field_1C              = b.get_uint2_le();
    total_reserved_blocks = b.get_uint2_le();

    for (int i=0; i<  6; i++) field_20[i]                = b.get_uint1();
    for (int i=0; i<820; i++) reserved_block_pool_map[i] = b.get_uint2_le();
    for (int i=0; i<  4; i++) vfl_context_block[i]       = b.get_uint2_le();

    usable_blocks_per_bank    = b.get_uint2_le();
    reserved_block_pool_start = b.get_uint2_le();

    for (int i=0; i<3; i++) 
        control_block[i] = b.get_uint2_le();

    scrub_list_length = b.get_uint2_le();

    for (int i=0; i<20; i++) 
        scrub_list[i] = b.get_uint2_le();

    for (int i=0; i<4; i++) 
        field_6CA[i] = b.get_uint4_le();

    vendor_type = b.get_uint4_le();

    for (int i=0; i<204; i++) 
        field_6DE[i] = b.get_uint1();

    remapping_schedule_start = b.get_uint2_le();

    for (int i=0; i<0x48; i++) 
        unk3[i] = b.get_uint1();

    version   = b.get_uint4_le();
    checksum1 = b.get_uint4_le();
    checksum2 = b.get_uint4_le();
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// TODO: 완벽 검증해야한다.
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
VSVFL::VSVFL(NAND const& n)
    : VFLBase(n)
{
    _banks_per_ce_vfl = 1;
    uint32_t vt[] = { 0x100010, 0x100014, 0x120014, 0x150011 };
    if (find_if(vt, vt+4, arg1 == _vendor_type) != vt+4)
        _banks_per_ce_vfl = 2;
    
    // begin override defaults
    _banks_total  = _ce_count * _banks_per_ce_vfl;
    _banks_per_ce = n.banks_per_ce_physical();

    _pages_per_sublk     = _pages_per_block * _banks_per_ce_vfl * _ce_count;
    _blocks_per_bank_vfl = _blocks_per_ce / _banks_per_ce_vfl;
    // end override defaults

    _bank_address_space  = _nand.bank_address_space();
    
    _vfl_contexts.clear();
    _bbts.clear();

    int reserved_blocks = 0;

    if (_nand.boot_from_nand())
        reserved_blocks = 16;

    int fs_start_block = reserved_blocks + 16;

    for (uint32_t ce=0; ce<_ce_count; ++ce)
    {
        VSVFLContext vflctx;
        for (uint32_t b=reserved_blocks; b<fs_start_block; ++b)
        {
            // TODO 검증한다. 꼭 ...
            auto page = _nand.read_meta_page(ce, b, 0, kVSVFLUserSpareData);
            if (page.data.empty())
                continue;
            
            vflctx.read_from(page.data);
            if (!util::vfl_check_checksum(page.data))
            {
                vflctx.clear();
                continue;
            }

            break;
        }

        if (vflctx.empty())
        {
            auto msg = str(format("Unable to find VSVFL context for CE %d") % ce);
            throw std::runtime_error(msg.c_str());
        }

        int32_t  most_recent_vfl_block = -1;
        uint32_t min_usn = 0xFFFFFFFF;

        // TODO DEBUG Here!!!
        for (auto i=0; i<4; i++)
        {
            auto b = vflctx.vfl_context_block[i];
            auto page = _nand.read_meta_page(ce, b, 0, kVSVFLUserSpareData);

            if (page.data.empty() || page.spare.empty())
                continue;
            
            VSVFLMetaSpareData s(page.spare);
            if (s.type1 != PAGETYPE_VFL)
                continue;

            if (0 < s.usnDec && s.usnDec <= min_usn)
            {
                min_usn = s.usnDec;
                most_recent_vfl_block = b;
            }
        }

        if (most_recent_vfl_block == -1)
        {
            // cout << "MostRecentVFLCxtBlock == -1";
            return;
        }

        ByteBuffer last;
        for (auto page_no=0; page_no<_pages_per_block; ++page_no)
        {
            auto page = _nand.read_meta_page(
                    ce, 
                    most_recent_vfl_block, 
                    page_no, 
                    kVSVFLUserSpareData);
            if (page.data.empty() || page.spare.empty())
                break;
            
            VSVFLUserSpareData s(page.spare);
            if (s.type1 != PAGETYPE_VFL)
                break;

            last = page.data;
        }

        vflctx.read_from(last);
        if (!util::vfl_check_checksum(last))
            cout << "VSVFL checksum FAIL";

        _vfl_contexts.push_back(vflctx);
        if (vflctx.version == 2 && vflctx.usn_inc > _current_version)
        {
            _current_version = vflctx.usn_inc;
            _usable_blocks_per_bank = vflctx.usable_blocks_per_bank;
            // _context = last;
        }
    }

    if (_vfl_contexts.size() == 0)
        throw std::runtime_error("VFL open FAIL");

    auto num_reserved     = _vfl_contexts[0].reserved_block_pool_start;
    auto num_non_reserved = _blocks_per_bank_vfl - num_reserved;
    for (auto ce=0; ce<_ce_count; ++ce)
    {
        auto sz = util::ceil_divide(_blocks_per_ce, 8);
        vector<uint8_t> bbt(sz, 0xFF);
        auto& ctx = _vfl_contexts[ce];
        for (auto bank=0; bank<_banks_per_ce_vfl; ++bank)
        {
            for (auto i=0; i<num_non_reserved; i++)
            {
                uint32_t pblock = 0;
                auto map_entry  = ctx.reserved_block_pool_map[bank*num_non_reserved + i];
                if (map_entry == 0xFFF0)
                    continue;

                if (map_entry < _blocks_per_ce)
                {
                    pblock = map_entry;
                }
                else if (map_entry > 0xFFF0)
                {
                    uint32_t vblock = ce + uint32_t(bank) * _ce_count;
                    pblock = virtual_block_to_physical_block(vblock, num_reserved + i);
                }
                else
                {
                    throw std::runtime_error("VSVFL: bad map table");
                }

                bbt[pblock/8] &= ~(1 << (pblock % 8));
            }
        }

        _bbts.push_back(bbt);
    }

    cout << "VSVFL context open OK";
}

auto VSVFL::get_ftl_ctrl_block() -> vector<uint16_t>
{
    vector<uint16_t> res;
    for (auto it=_vfl_contexts.begin(); it!=_vfl_contexts.end(); ++it)
    {
        if (it->usn_inc == _current_version)
        {
            res.assign(it->control_block, it->control_block + 3);
            break;
        }
    }
        
    return res;
}

auto VSVFL::virtual_to_physical(uint32_t vbank, uint32_t vpage) -> PhysicalAddr
{
    uint32_t vt[] = { 0x100010, 0x100014, 0x120014 };

    PhysicalAddr addr;

    // TODO BUG?  in the original source?
    if (_vendor_type == 0x10001)
    {
        addr.ce   = vbank;
        addr.page = vpage;
    }
    else if (_vendor_type == 0x150011)
    {
        auto pbank = vbank / _ce_count;
        auto ppage = ((_pages_per_block - 1) & vpage) |
                     (2 * (~(_pages_per_block - 1) & vpage));

        if (pbank & 1)
            ppage |= _pages_per_block;

        addr.ce   = vbank % _ce_count;
        addr.page = ppage;
    }
    else if (find_if(vt, vt+3, arg1 == _vendor_type) != vt+3)
    {
        auto pblock = 2 * (vpage / _pages_per_block);
        if (vbank % (2 * _ce_count) >= _ce_count)
            pblock += 1;

        addr.ce   = vbank % _ce_count;
        addr.page = _pages_per_block * pblock | (vpage  % 128);
    }
    else
    {
        auto msg = str(format("VSVFL: unsupported vendor 0x%x") % _vendor_type);
        throw std::runtime_error(msg.c_str());
    }

    return addr;
}

auto VSVFL::virtual_block_to_physical_block(uint32_t vbank, uint32_t vblock) -> uint32_t
{
    auto paddr = virtual_to_physical(vbank, _pages_per_block * vblock);
    return paddr.page / _pages_per_block;
}

auto VSVFL::is_good_block(vector<uint8_t> const& bbt, uint32_t block) -> bool
{
    if (block > _blocks_per_ce)
    {
        auto msg = str(format("vsvfl_is_good_block block %d out of bounds") % block);
        throw std::runtime_error(msg.c_str());
    }

    return (bbt[block / 8] & (1 << (block % 8))) != 0;
}

auto VSVFL::remap_block(uint32_t ce, uint32_t block) -> uint32_t
{
    if (is_good_block(_bbts[ce], block))
        return block;

    auto&  ctx = _vfl_contexts[ce];
    auto range = _blocks_per_ce - ctx.reserved_block_pool_start * _banks_per_ce_vfl;
    for (auto pw_des_pbn=0; pw_des_pbn<range; ++pw_des_pbn)
    {
        if (ctx.reserved_block_pool_map[pw_des_pbn] == block)
        {
            auto diff = _blocks_per_bank_vfl - ctx.reserved_block_pool_start;
            auto vbak = ce + _ce_count * (pw_des_pbn / diff);
            auto vblk = ctx.reserved_block_pool_start + (pw_des_pbn % diff);
            auto z    = virtual_block_to_physical_block(vbak, vblk);

            return z;
        }
    }

    cout << "bad block " << block << " not remapped\n";
    return block;
}

auto VSVFL::virtual_page_number_to_physical(uint32_t vpn) -> PhysicalAddr
{
    auto vbank  = vpn % _banks_total; 
    auto ce     = vbank % _ce_count;
    auto pblock = virtual_block_to_physical_block(vbank, vpn / _pages_per_sublk);
         pblock = remap_block(ce, pblock);

    auto bank_offset = _bank_address_space * (pblock / _blocks_per_bank);
    auto page   =  _pages_per_block_2 * (bank_offset + (pblock % _blocks_per_bank))
                     + ((vpn % _pages_per_sublk) / _banks_total);

    PhysicalAddr paddr;
    paddr.ce   = ce;
    paddr.page = page;

    return paddr;
}

auto VSVFL::read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t lpn) 
    -> NANDPage
{
    auto paddr = virtual_page_number_to_physical(vpn);

    return _nand.read_page(paddr.ce, paddr.page, key, lpn);
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
