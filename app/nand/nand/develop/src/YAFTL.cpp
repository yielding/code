#include "stdafx.h"

#include "YAFTL.h"
#include "NANDUtil.h"

#include <boost/format.hpp>
#include <iostream>

using namespace boost;
using namespace std;
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
namespace
{
    auto META_KEY = ByteBuffer::from_hexcode("92a742ab08c969bf006c9412d3cc79a5");
}

void YAFTLContext::read_from(ByteBuffer const& b)
{

}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
YAFTL::YAFTL(VSVFL* vsvfl, uint32_t usn)
    : _vfl(vsvfl)
{
    auto bytes_per_page  = _vfl->nand_page_size();
    _num_blocks_per_bank = _vfl->usable_blocks_per_bank();

    _blank_page.reset(bytes_per_page, 0x00);
    _toc_pages_per_block = _vfl->pages_per_sublk() * 4 / bytes_per_page;
    if (_vfl->pages_per_sublk() * 4 % bytes_per_page)
        _toc_pages_per_block++;
    
    _toc_entries_per_page = bytes_per_page / 4;
    _toc_array_length     = util::ceil_divide(
            _vfl->pages_per_sublk() * _num_blocks_per_bank * 4, bytes_per_page);

    _num_pages_toc_page_indices   = util::ceil_divide(_toc_array_length*4,  bytes_per_page);
    _num_pages_block_statuses     = util::ceil_divide(_num_blocks_per_bank, bytes_per_page);
    _num_pages_block_read_counts  = util::ceil_divide(_num_blocks_per_bank*2, bytes_per_page);
    _num_pages_block_erase_counts = util::ceil_divide(_num_blocks_per_bank*4, bytes_per_page);
    _num_pages_block_valid_pages_d_numbers = _num_pages_block_read_counts;
    _num_pages_block_valid_pages_i_numbers = _num_pages_block_read_counts;
    _ctrl_block_page_offset       = _num_pages_toc_page_indices 
                                  + _num_pages_block_statuses
                                  + _num_pages_block_read_counts
                                  + _num_pages_block_erase_counts
                                  + _num_pages_block_valid_pages_d_numbers
                                  + _num_pages_block_valid_pages_i_numbers
                                  + 2 * _toc_pages_per_block
                                  + 2;
    
    _total_pages = (_num_blocks_per_bank - 8) * 
                   (_vfl->pages_per_sublk() - _toc_pages_per_block);

    _user_pages_per_block = _vfl->pages_per_sublk() - _toc_pages_per_block;
    uint32_t max_usn = 0;
    int32_t  ftl_ctrl_block = -1;

    auto ctrl_blocks = _vfl->get_ftl_ctrl_block();

    for (auto b=ctrl_blocks.begin(); b != ctrl_blocks.end(); ++b)
    {
        auto page = yaftl_read_page(*b * _vfl->pages_per_sublk(), META_KEY);
        if (page.data.empty())
            continue;

        SpareData s(page.spare);
        auto page_usn = s.usn;
        if (usn != 0 && page_usn > usn)
            break;

        if (page_usn > max_usn)
        {
            max_usn = page_usn;
            ftl_ctrl_block = *b;
        }
    }

    if (ftl_ctrl_block == -1 || max_usn == 0)
    {
        throw std::runtime_error("ftl_ctrl_block not found, restore needed");
        yaftl_restore();
        return;
    }

    auto end = _vfl->pages_per_sublk() - _ctrl_block_page_offset;
    for (int i=0; i<end; )
    {
        auto index = ftl_ctrl_block * _vfl->pages_per_sublk() + i;
        auto page  = yaftl_read_page(index, META_KEY);
        if (page.data.empty())
        {
            if (yaftl_read_ctx_info(index))
                return;

            auto msg = str(format("YaFTL_readCtxInfo FAIL, restore needed maxusn=%d") % max_usn);
            cout << msg;
            yaftl_restore();
            return;
        }

        if (!page.spare.empty())
        {
            SpareData s(page.spare);
            if (s.usn > max_usn)
                max_usn = s.usn;
        }

        i += _ctrl_block_page_offset + 1;
    }

    cout << "YaFTL open fail\n";
    yaftl_restore();
}

auto YAFTL::yaftl_read_page(uint32_t page_no, ByteBuffer const& key, uint32_t lpn) -> NANDPage
{
    return _vfl->read_single_page(page_no, key, lpn);
}

void YAFTL::yaftl_restore()
{
}

bool YAFTL::yaftl_read_ctx_info(uint32_t page_no)
{
    auto page = yaftl_read_page(page_no, META_KEY);
    if (page.spare.empty())
        return false;

    SpareData s(page.spare);
    if (s.type != PAGETYPE_FTL_CLEAN)
        return false;

    YAFTLContext ctx(page.data);
    ctx.m_spare_usn = s.usn;      // spare_usn은 YAFTLContext안에 원래 없음..
    if (string(ctx.version, 4) != "CX01")
    {
        auto msg = str(format("Wrong FTL version %s") % ctx.version);
        cout << msg;
        return false;
    }

    _usn = s.usn;
    auto page_to_read = page_no + 1;
    auto user_toc_buffer = yaftl_read_n_page(page_to_read, _toc_pages_per_block);
    if (user_toc_buffer.empty())
        throw runtime_error("userTOCBuffer error");

    page_to_read += _toc_pages_per_block;
    auto index_toc_buffer = yaftl_read_n_page(page_to_read, _toc_pages_per_block);
    page_to_read += _toc_pages_per_block + 1;
    auto toc_array_index_pages = yaftl_read_n_page(page_to_read, _num_pages_toc_page_indices);

    while (toc_array_index_pages.has_remaining())
    {
        auto v = toc_array_index_pages.get_uint2_le();
        _toc_array_index_pages.push_back(v);
    }

    if (_toc_array_index_pages.size() != 4)
        throw runtime_error("Bad tocArrayIndexPage.size");

    _index_cache.clear();
    page_to_read += _num_pages_toc_page_indices;

    if (


    return false;
}

ByteBuffer YAFTL::yaftl_read_n_page(uint32_t page_to_read, uint32_t toc_pages_per_block)
{
    return ByteBuffer();
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
