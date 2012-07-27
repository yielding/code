#ifndef NANDIMAGEFLAT_H
#define NANDIMAGEFLAT_H

#include "NANDImage.h"

#include <map>
#include <string>
#include <fstream>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class NANDImageFlat: public NANDImage
{
public:
    NANDImageFlat(char const* filename, nand_info& g);

    virtual ~NANDImageFlat();

public:
    auto read_page(uint32_t ce_no, uint32_t page_no) -> NANDPage;
    
private:
    auto _read_page(uint32_t ce_no, uint32_t page_no) -> ByteBuffer;
    
private:
    std::ifstream _ifs;
    uint32_t      _nCEs;
    uint32_t      _page_size;
    uint32_t      _meta_size;
    int64_t       _dumped_page_size;
    int64_t       _image_size;
    bool          _has_iokit_status;
    ByteBuffer    _blank_page;
    ByteBuffer    _blank_spare;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
