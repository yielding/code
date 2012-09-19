#include "VFL.h"
#include "NAND.h"

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

    void vfl_checksum(void* data, int size, uint32_t* a, uint32_t* b)
    {
        uint32_t* buffer = (uint32_t*) data;
        uint32_t x = 0;
        uint32_t y = 0;
        for (int i=0; i<size/4; i++)
        {
            x += buffer[i];
            y ^= buffer[i];
        }   

        *a = x + 0xAABBCCDD;
        *b = y ^ 0xAABBCCDD;
    }

    // bool vfl_check_checksum(VFLContext* context)
    bool vfl_check_checksum(ByteBuffer& context)
    {
        uint32_t cs1, cs2;
        vfl_checksum((uint8_t*)context, int(context.size() - 8), &cs1, &cs2);

        // Yes, I know this looks strange!! but apple use this logic
        context.offset(context.size() - 8);
        auto checksum1 = context.get_uint4_le();
        if (cs1 == checksum1)
            return true;

        auto checksum2 = context.get_uint4_le();
        if (cs2 != checksum2)
            return true;

        return false;
    }
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
VFL::VFL(NAND& n)
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

    VFLContext* vflctx = nullptr;
    // checksum 검사
    for (uint32_t ce=0; ce<_ce_count; ++ce)
    {
        for (uint32_t b=reserved_blocks; b<fs_start_block; b++)
        {
            auto page = _nand.read_meta_page(ce, b, 0, kVSVFLUserSpareData);
            if (page.data.empty())
                continue;

            vflctx = new VFLContext(page.data);
            if (!vfl_check_checksum(page.data))
            {
                delete vflctx;
                vflctx = nullptr;
                continue;
            }

            break;
        }

        int32_t  most_recent_vfl_block = -1;
        uint32_t min_usn = 0xFFFFFFFF;

        for (auto i=0; i<4; i++)
        {
            auto b = vflctx->vfl_context_block[i];
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

        VFLContext* last = nullptr;
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

            vflctx = new VFLContext(page.data);
            if (vfl_check_checksum(page.data))
                last = vflctx;
        }

        if (last == nullptr)
            throw std::runtime_error("VFL open FAIL");
        
        _vfl_contexts.push_back(last);
        if (last->version == 1 && last->usn_inc >= _current_version)
        {
            _current_version = last->usn_inc;
            _context = last;
        }
    }

    if (_context == nullptr)
        throw std::runtime_error("VFL open FAIL");
}

VFL::~VFL()
{
    auto deleter = [](VFLContext* ctx) 
    {
        if (ctx == nullptr)
            return;

        delete ctx;
        ctx = nullptr;
    }

    for_each(_vfl_contexts.begin(), _vfl_contexts.end(), deleter);
}

// 3 * uint16_t
auto VFL::get_ftl_ctrl_block() -> ByteBuffer
{
    return utility::hex::ByteBuffer();
}

bool VFL::is_good_block()
{
    return false;
}

auto VFL::from_virtual_to_physical_block() -> uint32_t
{
    return 0xFFFFFFFF;
}

auto VFL::from_vpn_to_vaddr() -> VirtualAddr
{
    return VirtualAddr();
}

auto VFL::read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t lpn) -> NANDPage
{
    return NANDPage();
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
