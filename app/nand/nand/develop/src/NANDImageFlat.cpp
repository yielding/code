#include "stdafx.h"
#include "NANDImageFlat.h"
#include "DeviceInfo.h"

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
NANDImageFlat::NANDImageFlat(char const* filename, NandInfo& geometry)
{
    _filename  = filename;
    _nCEs      = geometry.ce_count;
    _page_size = geometry.bytes_per_page;
    _meta_size = geometry.meta_per_logical_page;
    _dumped_page_size = geometry.dumped_page_size;
    _has_iokit_status = true;

    auto sb = geometry.spare_byte_count;
    if (_dumped_page_size == _page_size + sb ||
        _dumped_page_size == _page_size + _meta_size)
    {
        _blank_page.reset(_page_size, 0xff);
        _blank_spare.reset(_meta_size, 0xff);
    }

    _image_size = fs::file_size(filename);
    uint32_t blocks_per_ce   = geometry.blocks_per_ce;
    uint32_t pages_per_block = geometry.pages_per_block;
    uint64_t expected_size   = _nCEs * blocks_per_ce * pages_per_block * _dumped_page_size;
    if (_image_size < expected_size)
      throw std::runtime_error("error: image appears to be truncated");

    /**/
    _ifs.open(filename, ios_base::binary);
    if (!_ifs.is_open())
        throw runtime_error("error: can't open the image file");
    /**/
}

NANDImageFlat::~NANDImageFlat()
{
}

auto NANDImageFlat::_read_page(uint32_t ce, uint32_t page) -> ByteBuffer
{
    auto index  = page * _nCEs + ce;
    auto offset = int64_t(index) * _dumped_page_size;
    if (offset + _dumped_page_size > _image_size)
        return ByteBuffer();

    ByteBuffer buffer(_dumped_page_size, 0);
    _ifs.seekg(offset, ios_base::beg);
    _ifs.read((char*)buffer, _dumped_page_size);
    
    if (_ifs.gcount() != _dumped_page_size)
    {
        _ifs.clear();
        return ByteBuffer();
    }
    
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
                  buffer.get_uint4_le();
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
