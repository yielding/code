#include "stdafx.h"
#include "VFL.h"
#include "NAND.h"
#include "NANDUtil.h"

#include <boost/format.hpp>

using namespace boost;

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// SupportedDevices
// https://github.com/iDroid-Project/openiBoot/blob/master/plat-s5l8900/includes/s5l8900/ftl.h
// https://github.com/iDroid-Project/openiBoot/blob/master/plat-s5l8900/ftl.c
// https://github.com/iDroid-Project/openiBoot/blob/master/plat-s5l8900/nand.c
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8

namespace {
    // from openiBoot/plat-s518900/includes/s518900/nand.h
    struct NANDDeviceType
    {
        uint32_t id;
        uint16_t blocks_per_bank;
        uint16_t pages_per_block;
        uint16_t sectors_per_page;
        uint16_t bytes_per_spare;
        uint8_t  wp_pulse_time;
        uint8_t  we_high_hold_time;
        uint8_t  nand_setting3;
        uint8_t  nand_setting4;
        uint32_t user_sublks_total;
        uint32_t ecc1;
        uint32_t ecc2;
    };

    static const NANDDeviceType SupportedDevices[] = {
        { 0x2555D5EC, 8192, 128, 4,  64, 4, 2, 4, 2, 7744, 4, 6},
        { 0xB614D5EC, 4096, 128, 8, 128, 4, 2, 4, 2, 3872, 4, 6},
        { 0xB655D7EC, 8192, 128, 8, 128, 4, 2, 4, 2, 7744, 4, 6},
        { 0xA514D3AD, 4096, 128, 4,  64, 4, 2, 4, 2, 3872, 4, 6},
        { 0xA555D5AD, 8192, 128, 4,  64, 4, 2, 4, 2, 7744, 4, 6},
        { 0xB614D5AD, 4096, 128, 8, 128, 4, 2, 4, 2, 3872, 4, 6},
        { 0xB655D7AD, 8192, 128, 8, 128, 4, 2, 4, 2, 7744, 4, 6},
        { 0xA585D598, 8320, 128, 4,  64, 6, 2, 4, 2, 7744, 4, 6},
        { 0xBA94D598, 4096, 128, 8, 216, 6, 2, 4, 2, 3872, 8, 8},
        { 0xBA95D798, 8192, 128, 8, 216, 6, 2, 4, 2, 7744, 8, 8},
        { 0x3ED5D789, 8192, 128, 8, 216, 4, 2, 4, 2, 7744, 8, 8},
        { 0x3E94D589, 4096, 128, 8, 216, 4, 2, 4, 2, 3872, 8, 8},
        { 0x3ED5D72C, 8192, 128, 8, 216, 4, 2, 4, 2, 7744, 8, 8},
        { 0x3E94D52C, 4096, 128, 8, 216, 4, 2, 4, 2, 3872, 8, 8}
    };
} 

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
void VFLContext::read_from(ByteBuffer const& b)
{
    usn_inc = b.get_uint4_le();
    for (int i=0; i<3; i++) 
        control_block[i] = b.get_uint2_le();

    unk1    = b.get_uint2_le();
    usn_dec = b.get_uint4_le();
    active_context_block      = b.get_uint2_le();
    next_context_page         = b.get_uint2_le();
    unk2                      = b.get_uint2_le();
    field16                   = b.get_uint2_le();
    field18                   = b.get_uint2_le();
    num_reserved_blocks       = b.get_uint2_le();
    reserved_block_bool_start = b.get_uint2_le();
    total_reserved_blocks     = b.get_uint2_le();;
    for (int i=0; i<820; i++) reserved_block_pool_map[i] = b.get_uint2_le();
    for (int i=0; i<282; i++) bad_block_table[i]         = b.get_uint1();
    for (int i=0; i<  4; i++) vfl_context_block[i]       = b.get_uint2_le();

    remapping_schedule_start = b.get_uint2_le();
    for (int i=0; i<0x48; i++) unk3[i] = b.get_uint1();
    version   = b.get_uint4_le();
    checksum1 = b.get_uint4_le();
    checksum2 = b.get_uint4_le();
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
VFL::VFL(NAND const& n)
    : _nand(n)
{
    _banks_total       = n.banks_total();
    _ce_count          = n.ce_count();
    _banks_per_ce      = n.banks_per_ce();
    _blocks_per_ce     = n.blocks_per_ce();
    _pages_per_block   = n.pages_per_block();
    _pages_per_block_2 = n.pages_per_block2();
    _pages_per_sublk     = _pages_per_block * _banks_per_ce * _ce_count;
    _blocks_per_bank     = _blocks_per_ce / _banks_per_ce;
    _blocks_per_bank_vfl = _blocks_per_ce / _banks_per_ce;
    _vendor_type         = n.vendor_type();
    _fs_start_block      = 5;

    // TODO REFACTOR: move method
    bool found = false;
    auto device_readid = n.device_readid();
    auto const      sz = sizeof(SupportedDevices) / sizeof(SupportedDevices[0]);
    for (int i=0; i<sz; i++)
    {
        if (SupportedDevices[i].id == device_readid)
        {
            found = true;
            break;
        }
    }

    if (!found)
    {
        auto msg = str(format("VFL: unsupported device 0x%x") % device_readid);
        assert(0);
    }

    auto  user_sublks_total = SupportedDevices[device_readid].user_sublks_total;
    _user_sublks_total      = user_sublks_total;
    auto user_pages_total   = user_sublks_total * _pages_per_sublk;
    auto sublks_total       = _blocks_per_ce;
    auto ftl_data_field_2   = sublks_total - user_sublks_total - 28;
        _ftl_data_field_4   = ftl_data_field_2 + 5;

    _vfl_contexts.clear();

    // TODO: Determine exact _bbt type
    _bbt.clear();
   
    _current_version = 0;
    uint32_t reserved_blocks = 0;
    uint32_t fs_start_block  = reserved_blocks + 10;

    // checksum 검사
    for (uint32_t ce=0; ce<_ce_count; ++ce)
    {
        VFLContext vflctx;
        for (uint32_t b=reserved_blocks; b<fs_start_block; b++)
        {
            auto page = _nand.read_meta_page(ce, b, 0, kVSVFLUserSpareData);
            if (page.data.empty())
                continue;

            vflctx.read_from(page.data);
            if (!util::vfl_check_checksum(page.data))
                continue;

            break;
        }

        int32_t  most_recent_vfl_block = -1;
        uint32_t min_usn = 0xFFFFFFFF;

        for (auto i=0; i<4; i++)
        {
            auto b = vflctx.vfl_context_block[i];
            // REMARK: kVSVFLMetaSpareData
            auto page = _nand.read_meta_page(ce, b, 0, kVSVFLMetaSpareData);
            if (page.data.empty())
                continue;

            VSVFLMetaSpareData s(page.spare);
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
            // REMARK: kVSVFLUserSpareData
            auto page = _nand.read_meta_page(
                            ce, 
                            most_recent_vfl_block, 
                            page_no, 
                            kVSVFLUserSpareData);
            if (page.data.empty())
                break;

            if (util::vfl_check_checksum(page.data))
                last = page.data;
        }

        if (last.empty())
            throw std::runtime_error("VFL open FAIL");
        
        VFLContext ctx(last);
        _vfl_contexts.push_back(ctx);
        if (ctx.version == 1 && ctx.usn_inc >= _current_version)
        {
            _current_version = ctx.usn_inc;
            _context = last;
        }
    }

    if (_context.empty())
        throw std::runtime_error("VFL open FAIL");
}

auto VFL::get_ftl_ctrl_block() -> vector<uint16_t>
{
    vector<uint16_t> res;
    for (auto it=_vfl_contexts.begin(); it!=_vfl_contexts.end(); ++it)
    {
        if (it->usn_inc == _current_version)
        {
            res.assign(it->control_block, it->control_block + 3);
            return res;
        }
    }
        
    return res;
}

bool VFL::is_good_block(uint8_t* bbt, uint32_t block)
{
    if (block > _blocks_per_ce)
        throw std::runtime_error("vfl is_good_block: block out of bound");

    auto index = block / 8;
    return ((bbt[index/8] >> (7 - (index % 8))) & 0x1) == 0x1;
}

auto VFL::virtual_block_to_physical_block(uint32_t ce, uint32_t physical_block) -> uint32_t
{
    if (is_good_block(_vfl_contexts[ce].bad_block_table, physical_block))
        return physical_block;

    auto& ctx = _vfl_contexts[ce];

    // REMARK: in org source i is pwDesPbn. What's it mean?
    for (auto i=0; i<ctx.num_reserved_blocks; i++)
    {
        if (ctx.reserved_block_pool_map[i] == physical_block)
            if (i > _blocks_per_ce)
            {
                throw std::runtime_error("Destination physical block for remapping is greater than number of blocks per bank!");
                return ctx.reserved_block_bool_start + i;
            }
    }

    return physical_block;
}

// TODO check the type of vpn
auto VFL::from_vpn_to_virtual_address(uint32_t vpn) -> VirtualAddr
{
    VirtualAddr va;

    va.bank  = vpn % _ce_count;
    va.block = vpn / _pages_per_sublk;
    va.page  = (vpn / _ce_count) % _pages_per_block;

    return va;
}

auto VFL::read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t lpn) -> NANDPage
{
    vpn += _pages_per_sublk * _ftl_data_field_4;
    auto va = from_vpn_to_virtual_address(vpn);
    auto pblock = virtual_block_to_physical_block(va.bank, va.block);

    return _nand.read_page(va.bank, pblock * _nand.pages_per_block() + va.page, key, lpn);
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
