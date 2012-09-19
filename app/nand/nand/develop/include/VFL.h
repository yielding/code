#ifndef VFL_H
#define VFL_H

#include "NANDCore.h"
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
struct NAND;

struct VirtualAddr
{
    uint32_t bank;
    uint32_t block;
    uint32_t page;
};

struct VFLContext
{
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

class VFL
{
public:
    VFL(NAND& n);

    auto get_ftl_ctrl_block() -> ByteBuffer;  // 3 * uint16_t

    bool is_good_block();
    auto from_virtual_to_physical_block() -> uint32_t;
    auto from_vpn_to_vaddr() -> VirtualAddr;
    auto read_single_page(uint32_t vpn, ByteBuffer const& key, uint32_t 
            lpn=0xffffffff) -> NANDPage;

private:
    NAND& _nand;

    uint32_t _banks_total;
    uint32_t _ce_count;
    uint16_t _banks_per_ce;
    uint32_t _blocks_per_ce;
    uint32_t _blocks_per_bank;
    uint32_t _blocks_per_bank_vfl;

    uint16_t _pages_per_block;
    uint16_t _pages_per_block_2;
    uint32_t _pages_per_sublk;
    uint32_t _vendor_type;
    uint32_t _fs_start_block;
    uint32_t _user_sublks_total;
    uint32_t _ftl_data_field_4;

    vector<VFLContext> _vfl_contexts;
    vector<int> _bbt;
    int _current_version;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
