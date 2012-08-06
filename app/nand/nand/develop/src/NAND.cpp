#include "stdafx.h"

#include "NAND.h"
#include "NANDImageFlat.h"
#include "DeviceInfo.h"

#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include <sstream>

using namespace std;
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace 
{
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
    //
    
    _boot_from_nand   = nand.boot_from_nand;
    _dumped_page_size = dumped_page_size;

    // TODO here
    // _page_size = nand
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
