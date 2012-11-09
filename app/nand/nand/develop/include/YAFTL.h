#ifndef YAFTL_H
#define YAFTL_H

#include "VSVFL.h"

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
/*
YAFTL_CXT = Struct("YAFTL_CXT",
    String("version", 4),
    ULInt32("unknCalculatedValue0"),
    ULInt32("totalPages"),
    ULInt32("latestUserBlock"),
    ULInt32("cxt_unkn0_usn"),
    ULInt32("latestIndexBlock"),
    ULInt32("maxIndexUsn"),
    ULInt32("blockStatsField4"),
    ULInt32("blockStatsField10"),
    ULInt32("numAllocatedBlocks"),
    ULInt32("numIAllocatedBlocks"),
    ULInt32("unk184_0xA"),
    Array(10, ULInt32("cxt_unkn1")),
    ULInt32("field_58"),
    ULInt16("tocArrayLength"),
    ULInt16("tocPagesPerBlock"),
    ULInt16("tocEntriesPerPage"),
 
    ULInt16("unkn_0x2A"),
    ULInt16("userPagesPerBlock"),
    ULInt16("unk64"),
    Array(11, ULInt32("cxt_unkn2")),
    ULInt8("unk188_0x63"),
)
*/

// 
// TODO REMARK 
// Check the difference between python code and original C code
//
struct YAFTLContext
{
    YAFTLContext() { }

    YAFTLContext(ByteBuffer const& b)
    {
        read_from(b);
    }

    void read_from(ByteBuffer const& b);

    uint32_t m_spare_usn;

    char     version[4];
    uint32_t unknCalculatedValue0;
    uint32_t totalPages;
    uint32_t latestUserBlock;
    uint32_t cxt_unkn0_usn;
    uint32_t latestIndexBlock;
    uint32_t maxIndexUsn;
    uint32_t blockStatsField4;
    uint32_t blockStatsField10;
    uint32_t numAllocatedBlocks;
    uint32_t numIAllocatedBlocks;
    uint32_t unk184_0xA;
    uint32_t cxt_unkn1[10];
    uint32_t field_58;
    uint16_t tocArrayLength;
    uint16_t tocPagesPerBlock;
    uint16_t tocEntriesPerPage;
    uint16_t unkn_0x2A;
    uint16_t userPagesPerBlock;
    uint16_t unk64;
    uint32_t cxt_unkn2[11];
    uint8_t  unk188_0x63;
};

struct TOCStruct
{
    uint32_t indexPage;
    uint16_t cacheNum;
    uint16_t TOCUnkMember2;
};

struct BlockStates
{
    uint32_t numAllocated;   // 00
    uint32_t numAvailable;   // 04
    uint32_t numValidDPages; // 08
    uint32_t numIAllocated;  // 0C
    uint32_t numIAvailable;  // 10
    uint32_t numValidIPages; // 14
    uint32_t numFree;        // 18
    uint32_t field_1C;       // 1C
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
class YAFTL
{
public:
    YAFTL(VSVFL* vsvfl, uint32_t usn=0);

public:
    bool yaftl_read_ctx_info(uint32_t page_no);
    void yaftl_restore();
    auto yaftl_read_page(uint32_t page_no, ByteBuffer const& key, 
                         uint32_t lpn=0xffffffff) -> NANDPage;

    auto yaftl_read_n_page(uint32_t page_to_read, uint32_t n, bool=false)
         -> ByteBuffer;

    auto read_btoc_pages(uint32_t b, uint32_t total_pages) 
         -> vector<uint32_t>;

    auto translate_lpn2vpn(uint32_t lpn) -> uint32_t;

private:
    map<uint32_t, uint32_t> _lpn2vpn;
    map<uint32_t, ByteBuffer> _index_cache;
    vector<uint16_t> _toc_array_index_pages;

    VSVFL*     _vfl;
    ByteBuffer _blank_page;

    uint32_t   _num_blocks_per_bank;
    uint32_t   _toc_pages_per_block;
    uint32_t   _toc_entries_per_page;
    uint32_t   _toc_array_length;
    uint32_t   _num_pages_toc_page_indices;
    uint32_t   _num_pages_block_statuses;
    uint32_t   _num_pages_block_read_counts;
    uint32_t   _num_pages_block_erase_counts;
    uint32_t   _num_pages_block_valid_pages_d_numbers;
    uint32_t   _num_pages_block_valid_pages_i_numbers;
    uint32_t   _ctrl_block_page_offset;
    uint32_t   _total_pages;
    uint32_t   _user_pages_per_block;
    uint32_t   _usn;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
