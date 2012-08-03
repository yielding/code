#ifndef NANDIMAGESPLITCES_H
#define NANDIMAGESPLITCES_H

#include "NANDImage.h"

#include <fstream>
#include <vector>
#include <string>
#include <map>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct nand_info;

class NANDImageSplitCEs: public NANDImage
{
public:
    NANDImageSplitCEs(char const* folder, nand_info const&);

    virtual ~NANDImageSplitCEs();

public:
    auto read_page(uint32_t ce_no, uint32_t page_no) -> NANDPage;
    
private:
    auto _read_page(uint32_t ce_no, uint32_t page_no) -> ByteBuffer;
    
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
