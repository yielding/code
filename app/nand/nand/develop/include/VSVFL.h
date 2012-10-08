#ifndef VSVFL_H
#define VSVFL_H

#include "NANDCore.h"
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
struct NAND;

struct VSVFLContext
{
    VSVFLContext() { _empty = true; }

    VSVFLContext(ByteBuffer const& b) 
    {
        read_from(b);
    }

    void read_from(ByteBuffer const& b);

    bool empty() const { return _empty; }
    void clear() const { _empty = true; }

    uint32_t usn_inc;
    uint32_t usn_dec;
    uint32_t ftl_type;
    uint16_t usn_block;
    uint16_t usn_page;
    uint16_t active_context_block;
    uint16_t write_failure_count;
    uint16_t bad_block_count;
    uint8_t  replaced_block_count[4];
    uint16_t num_reserved_blocks;
    uint16_t field_1C;
    uint16_t total_reserved_blocks;
    uint8_t  field_20[6];
    uint16_t reserved_block_pool_map[820];
    uint16_t vfl_context_block[4];
    uint16_t usable_blocks_per_bank;
    uint16_t reserved_block_pool_start;
    uint16_t control_block[3];
    uint16_t scrub_list_length;
    uint16_t scrub_list[20];
    uint32_t field_6CA[4];
    uint32_t vendor_type;
    uint8_t  field_6DE[204];
    uint16_t remapping_schedule_start;
    uint8_t  unk3[0x48];
    uint32_t version;
    uint32_t checksum1;
    uint32_t checksum2;

private:
    mutable bool _empty;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
class VSVFL : public IVFL
{
public:
    VSVFL(NAND const& n);
    virtual ~VSVFL() {}

public:
    auto get_ftl_ctrl_block() -> vector<uint16_t>;
    auto is_good_block(uint8_t* bbt, uint32_t block) -> bool;
    auto virtual_block_to_physical_block(uint32_t, uint32_t) -> uint32_t;
    auto read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t 
            lpn=0xffffffff) -> NANDPage;

    auto virtual_page_number_to_physical(uint32_t vpn) -> PhysicalAddr;
    auto remap_block(uint32_t ce, uint32_t block) -> uint32_t;

    auto virtual_to_physical(uint32_t, uint32_t) -> PhysicalAddr;

public:
    auto usable_blocks_per_bank() -> uint16_t { return _usable_blocks_per_bank; }

private:
    NAND const& _nand;

    uint32_t _ce_count;
    uint32_t _banks_total;
    uint32_t _banks_per_ce_vfl;
    uint32_t _banks_per_ce;
    uint32_t _blocks_per_ce;
    uint32_t _pages_per_block;
    uint32_t _pages_per_block_2;
    uint32_t _pages_per_sublk;
    uint32_t _blocks_per_bank;
    uint32_t _blocks_per_bank_vfl;
    uint32_t _vendor_type;
    uint32_t _bank_address_space;

    vector<VSVFLContext> _vfl_contexts;
    /*ByteBuffer _context;*/
    uint16_t _usable_blocks_per_bank;

    vector<vector<uint8_t>> _bbts;                // bbt: Bad Block Table
    int _current_version;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
