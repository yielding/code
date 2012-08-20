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
#include <map>
#include <fstream>
#include <sstream>

using utility::hex::ByteBuffer;
using std::string;
using std::ifstream;
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
struct SpareData
{
    SpareData(ByteBuffer& b) 
    {
        read_from(b);
    }

    void read_from(ByteBuffer& b)
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
