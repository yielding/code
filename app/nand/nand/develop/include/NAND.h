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

class NAND 
{
public:
    NAND(char const* fname, DeviceInfo& dinfo, int64_t ppn=0);

private:
    void init_geometry(std::map<std::string, std::string>& g);

private:
    NANDImage* _image;
    DeviceInfo& _dinfo;

    int  _ios_version;
    int  _meta_size;
    int64_t _dumped_page_size;
    bool _has_mbr;
    bool _metadata_whitening;
    bool _encrypted;

    std::string _filename;

    std::vector<uint32_t> _h2fmi_ht;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
