#ifndef VFLBASE_H
#define VFLBASE_H

#include "NANDCore.h"
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// VFLBase consists of common VFL interface and NAND interface
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
class NAND;

class VFLBase
{
public:
    VFLBase(NAND const& nand);

    virtual ~VFLBase() {}

public: // public interface
    virtual auto get_ftl_ctrl_block()                                -> vector<uint16_t> = 0;
    virtual auto is_good_block(uint8_t* bbt, uint32_t block)         -> bool             = 0;
    virtual auto virtual_block_to_physical_block(uint32_t, uint32_t) -> uint32_t         = 0;
    virtual auto read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t 
                                              lpn=0xffffffff)        -> NANDPage         = 0;
public: // nand interface
    auto nand_page_size()  const -> uint32_t;
    auto pages_per_sublk() const -> uint32_t;

protected:
    uint32_t _ce_count;
    uint32_t _banks_per_ce;
    uint32_t _blocks_per_ce;
    uint32_t _blocks_per_bank;
    uint32_t _blocks_per_bank_vfl;
    uint32_t _vendor_type;
    uint32_t _pages_per_block;
    uint32_t _pages_per_block_2;
    uint32_t _pages_per_sublk;
    uint32_t _banks_total;
    uint32_t _page_size;

    int      _current_version;

    NAND const& _nand;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
