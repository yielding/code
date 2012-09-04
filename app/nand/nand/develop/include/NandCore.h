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

#include <fstream>
#include <sstream>

using utility::hex::ByteBuffer;
using std::string;
using std::ifstream;
using std::map;
using std::vector;
using std::list;
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
// Spare Types
//
////////////////////////////////////////////////////////////////////////////////
enum { 
    kSpareData = 1,
    kVFLSpareData = 2,
}

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
    struct {
        uint32_t logicalPageNumber;
        uint32_t usn;
    } user;

    struct {
        uint32_t usnDec;
        uint16_t idx;
        uint8_t field_6;
        uint8_t field_7;
    } meta;
};
*/

struct VSVFLSpareData
{
    VSVFLSpareData(ByteBuffer const& b)
    {
    }

    void read_from(ByteBuffer const& b)
    {
    }

    ByteBuffer user_or_meta;
    uint8_t type2;
    uint8_t type1;
    uint8_t eccMark;
    uint8_t field_b;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
