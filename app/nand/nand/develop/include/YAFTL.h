#ifndef YAFTL_H
#define YAFTL_H

#include "NANDCore.h"
#include <string>

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

/*
struct YAFTLContext
{
    char[4]  version;                  // o 4
    uint32_t numIBlocks;               // o
    uint32_t totalPages;               // o
    uint32_t latestUserBlock;          // o
    uint32_t maxIndexUsn;              // o ctx_unkn0_usn
    uint32_t latestIndexBlk;           // o
    uint32_t maxIndexUsn2;             // o 18
    uint32_t numAvailableBlocks;       // 1C
    uint32_t numIAvailableBlocks;      // 20
    uint32_t numAllocatedBlocks;       // 24
    uint32_t numIAllocatedBlocks;      // 28
    uint32_t numCaches;                // 2C
    uint32_t field_30;                 // 30
    uint32_t cxt_unkn1[10];            // placeholder
    uint16_t tocArrayLength;           // 5C
    uint16_t tocPagesPerBlock;         // 5E
    uint16_t tocEntriesPerPage;        // 60
    uint16_t numFreeCaches;            // 62
    uint16_t field_64;                 // 64
    uint16_t pagesUsedInLatestUserBlk; // 66
    uint16_t pagesUsedInLatestIdxBlk;  // 68
    uint32_t cxt_unkn2[10];            // placeholder
    uint16_t field_92;                 // 92
    uint8_t  unk188_0x63;              // 94
    uint8_t  totalEraseCount;          // 95
};
// 37.2
*/

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
    YAFTL(IVFL* vsvfl);

private:
    IVFL* _vfl;
};

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
