#include "stdafx.h"

#include "NAND.h"
#include "NANDImageFlat.h"
#include "DeviceInfo.h"

#include <boost/algorithm/string.hpp>

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

void NAND::init_geometry(map<string, string> const& d)
{
    _meta_size = atoi(d["meta-per-logical-page"].c_str());
    if (_meta_size == 0)
        _meta_size = 12;

    _dumped_page_size = atol(d["dumpedPageSize"].c_str());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
