#include "stdafx.h"
#include "NANDImageFlat.h"
#include "ByteBuffer.h"

#include <boost/lexical_cast.hpp>

using namespace std;
using namespace boost;
using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
NANDImageFlat::NANDImageFlat(char const* filename, Geometry& geometry)
{
    _ifs.open(filename, ios_base::binary);
    _nCEs      = lexical_cast<int>(geometry["#ce"]);
    _page_size = lexical_cast<int>(geometry["#page-bytes"]);
    _meta_size = lexical_cast<int>(geometry["#meta-per-logical-page"]);
    auto value = geometry["#meta-per-logical-page"];
    _dumped_page_size = value.empty() ? _page_size + _meta_size + 8
                                      : lexical_cast<int>(value);
    _has_iokit_status = true;
    auto spare_types  = lexical_cast<int>(geometry["#spare-bytes"]);
    if ((_dumped_page_size == _page_size + spare_types) ||
        (_dumped_page_size == _page_size + _meta_size))
    {
        _blank_page  = ;
        _blank_spare = ;
    }

    
}

NANDImageFlat::~NANDImageFlat()
{
}

auto NANDImageFlat::read_page() -> ByteBuffer
{
    ByteBuffer buffer;

    return buffer;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
