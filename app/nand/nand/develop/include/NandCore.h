#ifndef NANDCORE_H
#define NANDCORE_H

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#include "ByteBuffer.h"  // vector, string, stdint.h, stdexcept

#include <algorithm>
#include <list>
#include <vector>
#include <map>
#include <string>
#include <stdint.h>

#include <fstream>
#include <sstream>

using utility::hex::ByteBuffer;
using std::string;
using std::ifstream;
using std::map;
using std::vector;
using std::list;

uint32_t const PAGETYPE_INDEX     =  0x4; // Index block indicator
uint32_t const PAGETYPE_LBN       = 0x10; // User data
uint32_t const PAGETYPE_FTL_CLEAN = 0x20; // FTL context (unmounted, clean)
uint32_t const PAGETYPE_VFL       = 0x80; // VFL context

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct NANDPage
{
    bool empty()     
    { 
        return spare.size() + data.size() == 0; 
    }

    bool blank() 
    {
        if (empty()) 
            return false;

        return  data.all_values_are(0xff) &&
               spare.all_values_are(0xff);
    }

    ByteBuffer spare;
    ByteBuffer data;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct VirtualAddr
{
    uint32_t bank;
    uint32_t block;
    uint32_t page;
};

struct PhysicalAddr
{
    uint32_t ce;
    uint32_t page;
};

////////////////////////////////////////////////////////////////////////////////
//
// Spare Types: TODO Is there any nicer solution than using this tags?
//
////////////////////////////////////////////////////////////////////////////////
enum SpareType {
    kSpareData = 1,
    kVSVFLUserSpareData,
    kVSVFLMetaSpareData
};

struct SpareData
{
    SpareData(ByteBuffer const& b) 
    {
        read_from(b);
    }

    void read_from(ByteBuffer const& b)
    {
        lpn     = b.get_uint4_le();
        usn     = b.get_uint4_le();
        field_8 = b.get_uint1();
        type    = b.get_uint1();
        field_a = b.get_uint2_le();
    }

    uint32_t lpn;
    uint32_t usn;
    uint8_t  field_8;
    uint8_t  type;
    uint16_t field_a;
};

/*
struct VSVFLSpareData {
    union {
        struct 
        {
            uint32_t logicalPageNumber;
            uint32_t usn;
        } user;

        struct 
        {
            uint32_t usnDec;
            uint16_t idx;
            uint8_t field_6;
            uint8_t field_7;
        } meta;
    } foo;

    uint8_t type2;
    uint8_t type1;
    uint8_t eccMark;
    uint8_t field_B;
};
*/

struct VSVFLUserSpareData
{
    VSVFLUserSpareData(ByteBuffer const& b)
    {
        read_from(b);
    }
    
    void read_from(ByteBuffer const& b)
    {
        logial_page_number = b.get_uint4_le();
        usn = b.get_uint4_le();

        type2   = b.get_uint1();
        type1   = b.get_uint1();
        eccMark = b.get_uint1();
        field_b = b.get_uint1();
    }

    uint32_t logial_page_number;
    uint32_t usn;

    uint8_t  type2;
    uint8_t  type1;
    uint8_t  eccMark;
    uint8_t  field_b;
};

struct VSVFLMetaSpareData
{
    VSVFLMetaSpareData(ByteBuffer const& b)
    {
        read_from(b);
    }
    
    void read_from(ByteBuffer const& b)
    {
        usnDec  = b.get_uint4_le();
        idx     = b.get_uint2_le();
        field_6 = b.get_uint1();;
        field_7 = b.get_uint1();;

        type2   = b.get_uint1();
        type1   = b.get_uint1();
        eccMark = b.get_uint1();
        field_b = b.get_uint1();
    }

    uint32_t usnDec;
    uint16_t idx;
    uint8_t  field_6;
    uint8_t  field_7;

    uint8_t  type2;
    uint8_t  type1;
    uint8_t  eccMark;
    uint8_t  field_b;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
