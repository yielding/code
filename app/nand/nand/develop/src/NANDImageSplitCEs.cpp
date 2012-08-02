#include "NANDImageSplitCEs.h"

#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>

using namespace std;
using namespace boost;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
NANDImageSplitCEs::NANDImageSplitCEs(char const* folder, Geometry& geometry)
{
    _nCEs      = lexical_cast<int>(geometry["#ce"]);
    _page_size = lexical_cast<int>(geometry["#page-bytes"]);
    _meta_size = 12;
    _npages    = 0;
    _dumped_page_size = 1 + _page_size + _meta_size;

    for (int i=0; i<_nCEs; ++i)
    {
        string path = str(format("%s/ce_%d.bin") % folder % i); 
        _paths.push_back(path);
        // TODO REVIEW
        _npages += fs::file_size(path) / _dumped_page_size;
    }
}

NANDImageSplitCEs::~NANDImageSplitCEs()
{
}

// TODO REVIEW
auto NANDImageSplitCEs::_read_page(uint32_t ce_no, uint32_t page_no) 
    -> ByteBuffer
{
    ByteBuffer buffer;

    if (ce_no >= _paths.size())
        return buffer;

    auto path = _paths[ce_no];
    if (!fs::exists(path))
        return buffer;

    auto offset = 8 + page_no * _dumped_page_size;
    if (offset >= fs::file_size(path))
        return buffer;

    ifstream ifs(path.c_str(), ios_base::binary);
    ifs.seekg(offset);

    buffer.reset(_dumped_page_size, 0);
    ifs.read((char*)buffer, _dumped_page_size);

    return buffer;
}
    
auto NANDImageSplitCEs::read_page(uint32_t ce_no, uint32_t page_no) 
    -> NANDPage
{
    NANDPage page;

    auto buffer = _read_page(ce_no, page_no);
    if (buffer.size() != _dumped_page_size)
        return page;

    if (!buffer[0] != '1' && buffer[0] != '\x01')
        return page;

    auto end = 1 + _page_size;
    page.data  = buffer.slice(1, end);
    page.spare = buffer.slice(end, end + _meta_size);

    return page;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
