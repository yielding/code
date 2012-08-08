#include "stdafx.h"

#include "NAND.h"
#include "NANDImageFlat.h"
#include "DeviceInfo.h"

#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include <sstream>
#include <cmath>
#include <map>

using namespace std;
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace 
{
    // 
    // TODO REFACTOR => move code position to another place
    // map<uint32_t, nand_info> nand_chip_info = {
    //
    nand_chip_info nc_infos[] = {
        // chipid
        { 0x7294D7EC,  0x1038,  0x80, 0x2000, 0x1B4,  0xC, 0, 8, 1, 0 },
        { 0x72D5DEEC,  0x2070,  0x80, 0x2000, 0x1B4,  0xC, 0, 8, 2, 0 },
        { 0x29D5D7EC,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 2, 2, 0 },
        { 0x2994D5EC,  0x1000,  0x80, 0x1000,  0xDA,    8, 0, 2, 1, 0 },
        { 0xB614D5EC,  0x1000,  0x80, 0x1000,  0x80,    4, 0, 2, 1, 0 },
        { 0xB655D7EC,  0x2000,  0x80, 0x1000,  0x80,    4, 0, 2, 2, 0 },
        { 0xB614D5AD,  0x1000,  0x80, 0x1000,  0x80,    4, 0, 3, 1, 0 },
        { 0x3294E798,  0x1004,  0x80, 0x2000, 0x1C0, 0x10, 0, 1, 1, 0 },
        { 0xBA94D598,  0x1000,  0x80, 0x1000,  0xDA,    8, 0, 1, 1, 0 },
        { 0xBA95D798,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 1, 2, 0 },
        { 0x3294D798,  0x1034,  0x80, 0x2000, 0x178,    8, 0, 1, 1, 0 },
        { 0x3295DE98,  0x2068,  0x80, 0x2000, 0x178,    8, 0, 1, 2, 0 },
        { 0x3295EE98,  0x2008,  0x80, 0x2000, 0x1C0, 0x18, 0, 1, 2, 0 },
        { 0x3E94D789,  0x2000,  0x80, 0x1000,  0xDA, 0x10, 0, 5, 1, 0 },
        { 0x3ED5D789,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 6, 2, 0 },
        { 0x3ED5D72C,  0x2000,  0x80, 0x1000,  0xDA,    8, 0, 5, 2, 0 },
        { 0x3E94D72C,  0x2000,  0x80, 0x1000,  0xDA,  0xC, 0, 7, 1, 0 },
        { 0x4604682C,  0x1000, 0x100, 0x1000,  0xE0,  0xC, 0, 7, 1, 0 },
        { 0x3294D745,  0x1000,  0x80, 0x2000, 0x178,    8, 0, 9, 1, 0 },
        { 0x3295DE45,  0x2000,  0x80, 0x2000, 0x178,    8, 0, 9, 2, 0 },
        { 0x32944845,  0x1000,  0x80, 0x2000, 0x1C0,    8, 0, 9, 1, 0 },
        { 0x32956845,  0x2000,  0x80, 0x2000, 0x1C0,    8, 0, 9, 2, 0 }
    };

    vector<uint32_t> gen_h2fmi_hash_table()
    {
        vector<uint32_t> res(256, 0);

        uint32_t val = 0x50F4546A;
        for (int i=0; i<256; i++)
        {
            val = 0x19660D * val + 0x3C6EF35F;

            for (int j=1; j<763; j++)
                val = (0x19660D * val) + 0x3C6EF35F;

            res[i] = val;
        }

        return res;
    }
}

namespace util
{
    string sizeof_fmt(int64_t size)
    {
        char const* fmts[] = { "bytes", "KB", "MB", "GB", "TB" };
        int const ARR_SIZE = sizeof(fmts) / sizeof(fmts[0]);
        string res;

        for (int i=0; i<ARR_SIZE; i++)
        {   
            if (size < 1024.0)
            {
                res = str(format("%3.1f %s") % double(size) % fmts[i]);
                break;
            }

            size /= 1024.0;
        }   

        return res;
    }
    
    uint32_t next_power_of_two(uint32_t value)
    {
        uint32_t res = 1;
        
        while (res < value) res <<= 1;
        
        return res;
    }
    
    double log2(double arg)
    {
        return log(arg) / log(2);
    }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
NAND::NAND(char const* fname, DeviceInfo& dinfo, int64_t ppn)
    : _filename(fname)
    , _dinfo(dinfo)
{
    _h2fmi_ht = gen_h2fmi_hash_table();

    // TODO REFACTOR
    auto nc_size = sizeof(nc_infos) / sizeof(nc_infos[0]);
    for (int i=0; i<nc_size; i++)
    {
        auto key = nc_infos[i].chip_id;
        _nand_chip_info[key] = nc_infos[i];
    }

    // TODO Check
    // _partition_table = nullptr;
    // _locker = nullptr;

    _ios_version = 0; 
    _has_mbr     = false;
    _metadata_whitening = false;

    char const* models[] = { "M68AP", "N45AP", "N82AP", "N72AP" };
    _encrypted 
        = find(models, models+4, _dinfo.hw_model()) == models+4;

    auto nand = dinfo.nand();
    init_geometry(nand);

    if (starts_with(_filename, "ce"))
    {
        // image = new NANDImageSplitCEs();
    }
    else if (_filename == "remote")
    {
        // image = new NANDImageRemote();
    }
    else
    {
        _image = new NANDImageFlat(_filename.c_str(), nand);
    }
    
    auto page = _image->read_page(0, 0);

}

NAND::~NAND()
{
    if (_image)
        delete _image;
}

void NAND::init_geometry(nand_info const& nand)
{
    _meta_size = nand.meta_per_logical_page;
    if (_meta_size == 0)
        _meta_size = 12;

    auto dumped_page_size = nand.dumped_page_size;
    if (dumped_page_size == 0)
        dumped_page_size = nand.bytes_per_page + _meta_size + 8;

    _dump_size = nand.ce_count * nand.blocks_per_ce * 
                 nand.pages_per_block * dumped_page_size;
    _total_pages   = nand.ce_count * nand.blocks_per_ce * nand.pages_per_block;
    auto nand_size = _total_pages * nand.bytes_per_page * nand.bytes_per_page;

    auto hsize = util::sizeof_fmt(nand_size);
    
    _bfn = nand.boot_from_nand;
    _dumped_page_size = dumped_page_size;

    _page_size = nand.bytes_per_page;
    _bootloader_bytes = nand.bootloader_bytes;
    if (_bootloader_bytes == 0)
        _bootloader_bytes = 1536;

    _empty_bootloader_page.resize(_bootloader_bytes, 0xff);
    _blank_page.resize(_page_size, 0xff);
    
    _ce_count           = nand.ce_count;
    _blocks_per_ce      = nand.blocks_per_ce;
    _pages_per_block    = nand.pages_per_block;
    _pages_per_ce       = _pages_per_block * _blocks_per_ce;
    _vendor_type        = nand.vendor_type;
    _device_readid      = nand.device_readid;
    _banks_per_ce_vfl   = nand.banks_per_ce;
    _metadata_whitening = nand.metadata_whitening;

    if (_nand_chip_info.find(_device_readid) != _nand_chip_info.end())
        _banks_per_ce_physical = _nand_chip_info[_device_readid].banks_per_ce;
    else
        _banks_per_ce_physical = 1;

    _blocks_per_bank = _blocks_per_ce / _banks_per_ce_physical;
    
    if ((_blocks_per_ce & _blocks_per_ce - 1) == 0)
    {
        _bank_address_space = _blocks_per_bank;
        _total_block_space  = _blocks_per_ce;
    }
    else
    {
        _bank_address_space = util::next_power_of_two(_blocks_per_bank);
        _total_block_space  = (_banks_per_ce_physical - 1) * _bank_address_space
                              + _blocks_per_bank;
    }
    
    _bank_mask = (int) util::log2(_bank_address_space * _pages_per_block);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
