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
    struct NANDDeviceType {
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
        //throw std::runtime_error(msg.c_str());
    }

    auto  user_sublks_total = SupportedDevices[device_readid].user_sublks_total;
    _user_sublks_total      = user_sublks_total;
    auto user_pages_total   = user_sublks_total * _pages_per_sublk;
    auto sublks_total       = _blocks_per_ce;
    auto ftl_data_field_2   = sublks_total - user_sublks_total - 28;
        _ftl_data_field_4   = ftl_data_field_2 + 5;

    _vfl_contexts.clear();

    // TODO: _bbt type
    _bbt.clear();
   
    _current_version = 0;
    uint32_t reserved_blocks = 0;
    uint32_t fs_start_block  = reserved_blocks + 10;

    // TODO here
    for (uint32_t ce=0; ce<_ce_count; ++ce)
    {
        for (uint32_t b=reserved_blocks; b<fs_start_block; b++)
        {
            _nand.read_meta_page(ce, b, 0, kVFLMetaSpareData);
        }
    }
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
