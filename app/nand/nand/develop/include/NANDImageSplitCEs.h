#ifndef NANDIMAGESPLITCES_H
#define NANDIMAGESPLITCES_H

#include "NANDImage.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct NandInfo;

class NANDImageSplitCEs: public NANDImage
{
public:
    NANDImageSplitCEs(char const* folder, NandInfo const&);

    virtual ~NANDImageSplitCEs();

public:
    auto read_page(uint32_t ce_no, uint32_t page_no) -> NANDPage;
    
private:
    auto _read_page(uint32_t ce_no, uint32_t page_no) -> utility::hex::ByteBuffer;
    
private:
    std::vector<std::string> _paths;
    uint32_t       _nCEs;
    uint32_t       _page_size;
    uint32_t       _meta_size;
    uint32_t       _npages;
    uint32_t       _dumped_page_size;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
