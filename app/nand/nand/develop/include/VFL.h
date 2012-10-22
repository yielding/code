#ifndef VFL_H
#define VFL_H

#include "VFLBase.h"

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
struct NAND;

struct VFLContext
{
    VFLContext() {}

    VFLContext(ByteBuffer const& b) 
    {
        read_from(b);
    }

    void read_from(ByteBuffer const& b);

    uint32_t usn_inc;
    uint16_t control_block[3];
    uint16_t unk1;
    uint32_t usn_dec;
    uint16_t active_context_block;
    uint16_t next_context_page;
    uint16_t unk2;
    uint16_t field16;
    uint16_t field18;
    uint16_t num_reserved_blocks;
    uint16_t reserved_block_bool_start;
    uint16_t total_reserved_blocks;
    uint16_t reserved_block_pool_map[820];
    uint8_t  bad_block_table[282];
    uint16_t vfl_context_block[4];
    uint16_t remapping_schedule_start;
    uint8_t  unk3[0x48];
    uint32_t version;
    uint32_t checksum1;
    uint32_t checksum2;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
class VFL: public VFLBase
{
public:
    VFL(NAND const& n);
    virtual ~VFL() {}

public:
    auto get_ftl_ctrl_block() -> vector<uint16_t>;  // 3 * uint16_t

    auto is_good_block(uint8_t* bbt, uint32_t block) -> bool;
    auto virtual_block_to_physical_block(uint32_t, uint32_t) -> uint32_t;
    auto read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t 
            lpn=0xffffffff) -> NANDPage;

public:
    auto from_vpn_to_virtual_address(uint32_t) -> VirtualAddr;

private:
    uint32_t _fs_start_block;
    uint32_t _user_sublks_total;
    uint32_t _ftl_data_field_4;

    vector<VFLContext> _vfl_contexts;
    ByteBuffer _context;

    vector<uint8_t> _bbt;                // bbt: Bad Block Table
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
