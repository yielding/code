#ifndef NAND_H
#define NAND_H

#include "NANDCore.h"
#include "NANDUtil.h"
////////////////////////////////////////////////////////////////////////////////
//
// a Fasade interface 
//
////////////////////////////////////////////////////////////////////////////////
class NANDImage;
class DeviceInfo;
class EffaceableLockers;
class YAFTL;
class VSVFL;

// TODO REFACTOR
struct NandInfo;

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
    NAND(char const* fname, DeviceInfo& dinfo, int64_t ppn=-1);
   ~NAND();

public:
    auto read_page(uint32_t ce, uint32_t page, 
                   ByteBuffer const& key=ByteBuffer(), 
                   uint32_t lpn=0xffffffff, SpareType = kSpareData) const
      -> NANDPage;
    
    auto read_special_pages(uint32_t ce_no, vector<string>& magics)
      -> map<string, ByteBuffer>;
    
    auto read_block_page(uint32_t ce, uint32_t block, uint32_t page,
                         ByteBuffer const&, 
                         uint32_t=0xffffffff, SpareType = kSpareData) const
      -> NANDPage;

    auto read_meta_page(uint32_t ce, uint32_t block, uint32_t page, SpareType st) const
      -> NANDPage;

public:
    auto banks_total() const      -> uint32_t { return _ce_count * _banks_per_ce_vfl; }
    auto ce_count() const         -> uint32_t { return _ce_count;                     }
    auto banks_per_ce() const     -> uint32_t { return _banks_per_ce_physical;        }
    auto blocks_per_ce() const    -> uint32_t { return _blocks_per_ce;                }
    auto pages_per_block() const  -> uint32_t { return _pages_per_block;              }
    auto pages_per_block2() const -> uint32_t { return util::next_power_of_two(_pages_per_block); }
    auto vendor_type() const      -> uint32_t { return _vendor_type;                  }
    auto device_readid() const    -> uint64_t { return _device_readid;                }
    auto banks_per_ce_physical() const -> uint32_t { return _banks_per_ce_physical;   }
    auto bank_address_space() const -> uint32_t    { return _bank_address_space;      }
    auto boot_from_nand() const -> bool            { return _bfn;                     }
    auto page_size() const -> uint32_t             { return _page_size;               } 

private:
    void init_geometry(NandInfo const& n);

    auto unwhiten_metadata(ByteBuffer& , uint32_t page_no) const -> ByteBuffer;
    auto decrypt_page(ByteBuffer data, ByteBuffer const& key, uint32_t page_no) const
      -> ByteBuffer;
    
    auto unpack_spacial_page(ByteBuffer& data) -> ByteBuffer;
    auto iv_for_page(uint32_t page_no) const -> ByteBuffer;

    auto find_lockers_unit() -> ByteBuffer;

private:
    DeviceInfo& _dinfo;
    NANDImage*  _image;
    VSVFL*      _vfl;
    YAFTL*      _ftl;

    int         _ios_version;
    int         _meta_size;
    int64_t     _dumped_page_size;
    bool        _has_mbr;
    bool        _metadata_whitening;
    bool        _encrypted;
    bool        _nand_only;

    int64_t     _dump_size;
    uint32_t    _total_pages;

    string      _filename;
    bool        _bfn;
    uint32_t    _page_size;
    uint32_t    _bootloader_bytes;
    uint32_t    _ce_count;
    uint32_t    _blocks_per_ce;
    uint32_t    _pages_per_block;
    uint32_t    _pages_per_ce;
    uint32_t    _vendor_type;
    uint64_t    _device_readid;
    uint32_t    _banks_per_ce_vfl;
    uint32_t    _banks_per_ce_physical;
    uint32_t    _blocks_per_bank;
    uint32_t    _bank_address_space;
    uint32_t    _total_block_space;
    int32_t     _bank_mask;

    // REMARK: these values are used when _dinfo does not 
    // have "lockers" key
    string      _emf;
    string      _dkey;

    EffaceableLockers* _lockers;
    
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
