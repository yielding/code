#include "stdafx.h"

#include "YAFTL.h"
#include "NANDUtil.h"
#include "NAND.h"

#include <boost/format.hpp>
#include <iostream>
#include <map>

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
    for (int i=0; i<4; i++) version[i] = b.get_int1();
    unknCalculatedValue0 = b.get_uint4_le();
    totalPages           = b.get_uint4_le();
    latestUserBlock      = b.get_uint4_le();
    cxt_unkn0_usn        = b.get_uint4_le();
    latestIndexBlock     = b.get_uint4_le();
    maxIndexUsn          = b.get_uint4_le();
    blockStatsField4     = b.get_uint4_le();
    blockStatsField10    = b.get_uint4_le();
    numAllocatedBlocks   = b.get_uint4_le();
    numIAllocatedBlocks  = b.get_uint4_le();
    unk184_0xA           = b.get_uint4_le();
    for (int i=0; i<10; i++) cxt_unkn1[i] = b.get_uint4_le();
    field_58             = b.get_uint4_le();
    tocArrayLength       = b.get_uint2_le();
    tocPagesPerBlock     = b.get_uint2_le();
    tocEntriesPerPage    = b.get_uint2_le();
    unkn_0x2A            = b.get_uint2_le();
    userPagesPerBlock    = b.get_uint2_le();
    unk64                = b.get_uint2_le();
    for (int i=0; i<11; i++) cxt_unkn2[i] = b.get_uint4_le();;
    unk188_0x63          = b.get_uint1();
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
    _ctrl_block_page_offset 
        = _num_pages_toc_page_indices 
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
        auto index = ftl_ctrl_block * _vfl->pages_per_sublk() + i + _ctrl_block_page_offset;
        auto page  = yaftl_read_page(index, META_KEY);
        if (page.data.empty())
        {
            if (yaftl_read_ctx_info(index))
                return;

            cout << str(format("YaFTL_readCtxInfo FAIL, restore needed maxusn=%d") % max_usn);
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

void YAFTL::yaftl_restore()
{
    //
    // REMARK : To call load_cached_data member function, we should include NAND.h
    //
    auto const& nd = _vfl->nand();
    _lpn2vpn = nd.load_cached_data("yaftlrestore.1");
    if (!_lpn2vpn.empty())
    {
        cout << "Found cached FTL restore information\n";
        return;
    }

    NANDCache user_blocks, index_blocks;
    for (uint32_t b=0; b<_num_blocks_per_bank; b++)
    {
        auto page = yaftl_read_page(b * _vfl->pages_per_sublk(), META_KEY);
        if (page.spare.empty())
            continue;

        SpareData s(page.spare);
        if (s.type == PAGETYPE_INDEX)
        {
            index_blocks[s.usn] = b;
        }
        else if (s.type == PAGETYPE_LBN)
        {
            auto res = user_blocks.insert(pair<uint32_t, uint32_t>(s.usn, b));
            if (!res.second)
                throw runtime_error("Two blocks with same USN, something is weird");
        }
        else if (s.type == PAGETYPE_FTL_CLEAN)
        {
            // Do nothing! in the original code
        }
        else
        {
            throw runtime_error("undefined page type!");
        }
    }

    NANDCache lpn2vpn;
    for (auto it = user_blocks.rbegin(); it != user_blocks.rend(); ++it)
    {
        auto usn  = it->first;
        auto b    = user_blocks[usn];
        auto btoc = read_btoc_pages(b, _total_pages);
        if (!btoc.empty())
        {
            for (int i=_user_pages_per_block-1; i>=0; --i)
            {
                auto key = btoc[i];
                auto val = b * _vfl->pages_per_sublk() + i;
                // NOTICE: contrary to []= operator, insert only applies where
                //         there is no same key exists.
                lpn2vpn.insert(pair<uint32_t, uint32_t>(key, val));
            }
        }
        else
        {
            auto msg = str(format("BTOC not found for block %d (usn %d), scanning all pages\n")
                           % b % usn);
            cout << msg;

            uint32_t i = 0;
            int beg = _vfl->pages_per_sublk() - _toc_pages_per_block - 1;
            for (int p = beg; p >= 0; --p)
            {
                auto page_no = b * _vfl->pages_per_sublk() + p;
                auto page    = yaftl_read_page(page_no, META_KEY);
                if (!page.spare.empty())
                {
                    i += 1;
                    SpareData s(page.spare);
                    lpn2vpn.insert(pair<uint32_t, uint32_t>(s.lpn, page_no));
                }
            }

            cout << str(format("%d used pages in block") % i);
        }
    }

    // REMARK: data test
    /*
    ofstream tmp("/Users/yielding/Desktop/a.txt");
    for (auto it=lpn2vpn.begin(); it != lpn2vpn.end(); ++it)
    {
        auto line = str(format("%d, %d\n") % it->first % it->second);
        tmp << line;
    }
    tmp.close();
    */
    
    _vfl->nand().save_cache_data("yaftlrestore.1", lpn2vpn);
    _lpn2vpn.swap(lpn2vpn);
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
    // NOTICE!!
    //   spare_usn은 YAFTLContext안에 원래 없음..
    ctx.m_spare_usn = s.usn;      
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

    if (false)
    {
        /*
        blockStatuses = self.YAFTL_read_n_Page(pageToRead, self.nPagesBlockStatuses);
        pageToRead += self.nPagesBlockStatuses;
        blockReadCounts = self.YAFTL_read_n_Page(pageToRead, self.nPagesBlockReadCounts);
        pageToRead += self.nPagesBlockReadCounts;
        blockEraseCounts = self.YAFTL_read_n_Page(pageToRead, self.nPagesBlockEraseCounts);
        pageToRead += self.nPagesBlockEraseCounts;
        validPagesINo = self.YAFTL_read_n_Page(pageToRead, self.nPagesBlockValidPagesINumbers);
        pageToRead += self.nPagesBlockValidPagesINumbers;
        validPagesDNo = self.YAFTL_read_n_Page(pageToRead, self.nPagesBlockValidPagesDNumbers);
        */
    }

    auto msg = str(format("YaFTL context OK, version=%s, maxIndexUsn=%d") 
                    % ctx.version % ctx.maxIndexUsn);

    cout << msg;
    return true;
}

ByteBuffer 
YAFTL::yaftl_read_n_page(uint32_t page_to_read, uint32_t n, bool failIfBlank)
{
    ByteBuffer result;

    for (auto i=0; i<n; i++)
    {
        auto page = yaftl_read_page(page_to_read + i, META_KEY);
        if (page.data.empty())
        {
            if (failIfBlank)
                return ByteBuffer();

            return result;
        }

        result.append(page.data);
    }

    return result;
}

NANDPage 
YAFTL::yaftl_read_page(uint32_t page_no, ByteBuffer const& key, uint32_t lpn)
{
    return _vfl->read_single_page(page_no, key, lpn);
}

uint32_t 
YAFTL::translate_lpn2vpn(uint32_t lpn)
{
    if (!_lpn2vpn.empty())
        return (_lpn2vpn.find(lpn) == _lpn2vpn.end())
            ? 0xffffffff
            : _lpn2vpn[lpn];

    uint32_t toc_page_no = lpn / _toc_entries_per_page;
    uint32_t index_page  = _toc_array_index_pages[toc_page_no];
    if (index_page == 0xffffffff)
        return 0xffffffff;

    IndexPages toc_page_buffer;
    if (_index_cache.find(index_page) != _index_cache.end())
    {
        toc_page_buffer = _index_cache[index_page];
    }
    else
    {
        auto page = yaftl_read_page(index_page, META_KEY);
        if (page.data.empty())
        {
            cout << "tocPageBuffer fail\n";
            return 0xffffffff;
        }

        SpareData s(page.spare);
        if (s.type != PAGETYPE_INDEX)
            throw runtime_error("Wrong Page Type!");

        for (size_t i=0; i<page.data.size(); i++)
            toc_page_buffer.push_back(uint16_t(page.data[i]));

        _index_cache[index_page] = toc_page_buffer;
    }

    // toc_entry;
    return toc_page_buffer[lpn % _toc_entries_per_page];
}

ByteBuffer 
YAFTL::read_lpn(uint32_t lpn, ByteBuffer key) 
{
    ByteBuffer blank;
    auto vpn = translate_lpn2vpn(lpn);
    if (vpn == 0xffffffff)
        return blank;

    auto page = yaftl_read_page(vpn, key, lpn);
    if (page.data.empty())
        return blank;

    SpareData s(page.spare);
    if (s.lpn != lpn)
    {
        auto msg = str(format("YAFTL translatioin FAIL spare lpn : %d vs expected %d")
                       % s.lpn % lpn);
        throw runtime_error(msg.c_str());
    }

    return page.data;
}

NANDCache2 YAFTL::yaftl_lookup1()
{
    auto cache = _vfl->nand().load_cached_data2("YAFTL_lookup1");
    
    if (!cache.first.empty() || !cache.second.empty())
    {
        cout << "Found cached FTL lookup table\n";
        return cache;
    }

    auto& usr_blk = cache.first;
    auto& idx_blk = cache.second;

    for (auto b=0; b<_num_blocks_per_bank; ++b)
    {
        auto page = yaftl_read_page(b * _vfl->pages_per_sublk() + 0, META_KEY);
        if (page.spare.empty())
            continue;

        SpareData s(page.spare);
        if (s.type == PAGETYPE_INDEX)
        {
            idx_blk[s.usn] = b;
        }
        else if (s.type == PAGETYPE_LBN)
        {
            auto res = usr_blk.insert(pair<uint32_t, uint32_t>(s.usn, b));
            if (!res.second)
                throw std::runtime_error("Two blocks with same USN, something is weird");
        }
        else if (s.type == PAGETYPE_FTL_CLEAN)
        {}
    }

    NANDCache lpn2vpn;

    for (auto it = usr_blk.rbegin(); it != usr_blk.rend(); ++it)
    {
        auto usn  = it->first;
        auto b    = usr_blk[usn];
        auto btoc = read_btoc_pages(b, _total_pages);

        if (!btoc.empty())
        {
            for (int i=_user_pages_per_block-1; i>=0; --i)
            {
                // TODO
                // lpn2vpn.setdefault <== 이거 어떻게 하는 거지?
            }
        }
        else
        {
            uint32_t i   = 0;
            int64_t usn_ = -1;
            int beg = _vfl->pages_per_sublk() - _toc_pages_per_block - 1;
            for (int p = beg; p >= 0; --p)
            {
                auto page_no = b * _vfl->pages_per_sublk() + p;
                auto page    = yaftl_read_page(page_no, META_KEY);
                if (page.spare.empty())
                    break;

                i += 1;
                SpareData s(page.spare);

                if (usn_ == -1 || usn_ != s.usn)
                    usn_ = s.usn;

                // TODO
                // lpn2vpn.setdefault <== 이거 어떻게 하는 거지?
            }

            cout << str(format("%d used pages in block") % i);
        }
    }

    return cache;
}

vector<uint32_t> 
YAFTL::read_btoc_pages(uint32_t block, uint32_t max_val)
{
    vector<uint32_t> btoc;

    ByteBuffer data;
    for (uint32_t i=0; i<_toc_pages_per_block; i++)
    {
        auto index = (block + 1) * _vfl->pages_per_sublk() - _toc_pages_per_block + i;
        auto page  = yaftl_read_page(index, META_KEY);
        if (page.spare.empty())
            return vector<uint32_t>();

        data.append(page.data);
    }

    data.flip();
    while (data.has_remaining())
    {
        auto val = data.get_uint4_le();
        if (val > max_val)
            val = max_val;
        btoc.push_back(val);
    }

    return btoc;
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
