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

using std::string;
using std::vector;
using std::map;

//
// 1. dinfo, nand가 중복
//
// 2. 여러 가지 type 정의를 어디에 둘것인지 결정
//    => pimpl
//
struct nand_chip_info
{
    uint32_t chip_id;
    uint16_t blocks_per_ce;
    uint16_t pages_per_block;
    uint16_t bytes_per_page;
    uint16_t bytes_per_spare;
    uint16_t unk5;
    uint16_t unk6;
    uint32_t unk7;
    uint16_t banks_per_ce;
    uint16_t unk9;
};

class NAND 
{
public:
    NAND(char const* fname, DeviceInfo& dinfo, int64_t ppn=0);
    ~NAND();

private:
    void init_geometry(nand_info const& n);

private:
    NANDImage*  _image;
    DeviceInfo& _dinfo;

    int         _ios_version;
    int         _meta_size;
    int64_t     _dumped_page_size;
    bool        _has_mbr;
    bool        _metadata_whitening;
    bool        _encrypted;

    int64_t     _dump_size;
    uint32_t    _total_pages;

    string      _filename;
    string      _bfn;
    uint32_t    _page_size;
    uint32_t    _bootloader_bytes;
    uint16_t    _ce_count;
    uint32_t    _blocks_per_ce;
    uint32_t    _pages_per_block;
    uint32_t    _pages_per_ce;
    uint32_t    _vendor_type;
    uint64_t    _device_readid;
    uint16_t    _banks_per_ce_vfl;
    uint32_t    _banks_per_ce_physical;
    uint32_t    _blocks_per_bank;
    uint32_t    _bank_address_space;
    uint32_t    _total_block_space;
    int32_t     _bank_mask;

    vector<uint8_t>  _empty_bootloader_page;
    vector<uint8_t>  _blank_page;
    vector<uint32_t> _h2fmi_ht;
    map<uint64_t, nand_chip_info> _nand_chip_info;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
