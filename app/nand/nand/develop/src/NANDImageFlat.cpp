#include "stdafx.h"
#include "NANDImageFlat.h"

#include <utility>
#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>

using namespace std;
using namespace boost;
using namespace utility::hex;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
NANDImageFlat::NANDImageFlat(char const* filename, Geometry& geometry)
{
    _ifs.open(filename, ios_base::binary);
    if (!_ifs.is_open())
        throw runtime_error("error: can't open the image file");
    
    _nCEs      = lexical_cast<int>(geometry["#ce"]);
    _page_size = lexical_cast<int>(geometry["#page-bytes"]);
    _meta_size = lexical_cast<int>(geometry["meta-per-logical-page"]);
    auto value = geometry["#meta-per-logical-page"];
    _dumped_page_size = value.empty() ? _page_size + _meta_size + 8
                                      : lexical_cast<int>(value);
    _has_iokit_status = true;
    auto spare_bytes  = lexical_cast<int>(geometry["#spare-bytes"]);
    if (_dumped_page_size == _page_size + spare_bytes ||
        _dumped_page_size == _page_size + _meta_size)
    {
        _blank_page.reset(_page_size, 0xff);
        _blank_spare.reset(_meta_size, 0xff);
    }

    _image_size = fs::file_size(filename);
    uint32_t blocks_per_ce   = lexical_cast<int>(geometry["#ce-blocks"]);
    uint32_t pages_per_block = lexical_cast<int>(geometry["#block-pages"]);
    uint64_t expected_size   = _nCEs * blocks_per_ce * pages_per_block * _dumped_page_size;
    if (_image_size < expected_size)
      throw std::runtime_error("error: image appears to be truncated");
}

NANDImageFlat::~NANDImageFlat()
{
}

auto NANDImageFlat::_read_page(uint32_t ce, uint32_t page) -> ByteBuffer
{
    ByteBuffer buffer;

    auto index  = page * _nCEs + ce;
    auto offset = int64_t(index) * _dumped_page_size;

    buffer.reset(_dumped_page_size, 0);
    _ifs.seekg(offset);
    _ifs.read((char*)buffer, _dumped_page_size);

    return buffer;
}

auto NANDImageFlat::read_page(uint32_t ce_no, uint32_t page_no) -> NANDPage
{
    NANDPage page;
    auto buffer = _read_page(ce_no, page_no);
    if (buffer.empty() || buffer.size() != _dumped_page_size)
        return page;

    if (_has_iokit_status)
    {
        auto beg = _page_size + _meta_size;
        buffer.offset(beg);
        auto r1 = buffer.get_uint4_le();
        auto r2 = buffer.get_uint4_le();
        if (r1 != 0)
            return page;
    }

    // REMARK check one more
    page.data  = buffer.slice(0, _page_size);
    page.spare = buffer.slice(_page_size, _page_size + _meta_size);
    if (!_has_iokit_status && page.blank())
        return NANDPage();

    return page;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
