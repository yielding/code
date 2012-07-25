#ifndef NANDIMAGE_H
#define NANDIMAGE_H

#include <string>
#include <map>

#include "ByteBuffer.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef std::map<std::string, std::string> Geometry;

using namespace utility::hex;

struct NANDPage
{
    ByteBuffer spare;
    ByteBuffer data;
    bool empty()     { return spare.size() + data.size() == 0; }

    bool blank() 
    {
        if (empty()) 
            return false;

        return data .all_values_are(0xff) &&
               spare.all_values_are(0xff);
    }
};

class NANDImage
{
public:
    virtual ~NANDImage()
    {}

public:
    auto virtual read_page(uint32_t ce_no, uint32_t page_no) -> NANDPage = 0;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
