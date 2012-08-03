#ifndef NAND_H
#define NAND_H

#include <stdint.h>
#include <string>
#include <vector>
#include <map>

////////////////////////////////////////////////////////////////////////////////
//
// a Fasade interface 
//
////////////////////////////////////////////////////////////////////////////////
class NANDImage;
class DeviceInfo;

struct nand_info;

class NAND 
{
public:
    NAND(char const* fname, DeviceInfo& dinfo, int64_t ppn=0);
    ~NAND();

private:
    void init_geometry(nand_info const& n);

private:
    NANDImage* _image;
    DeviceInfo& _dinfo;

    int  _ios_version;
    int  _meta_size;
    int64_t _dumped_page_size;
    bool _has_mbr;
    bool _metadata_whitening;
    bool _encrypted;

    int64_t _dump_size;
    uint32_t _total_pages;

    std::string _filename;

    std::vector<uint32_t> _h2fmi_ht;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
