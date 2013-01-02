#include "stdafx.h"

#include "ByteBuffer.h"
#include "EndianSwap.h"
#include <boost/algorithm/string.hpp>

#include <cstdlib>
#include <cstring>
#include <string>
#include <sstream>
#include <iomanip>

#if defined(WIN32) && defined(_DEBUG)
#define new DEBUG_NEW
#endif

using namespace std;
using namespace boost;

namespace utility { namespace hex {    
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
// this constructor is for network I/O
ByteBuffer::ByteBuffer(size_t size)
    : m_offset(0)
{
    m_buffer.resize(size);
}

// this constructor is for hex conversion
ByteBuffer::ByteBuffer(uint8_t* buffer, size_t sz)
{
    m_offset = 0;
    m_buffer.assign(buffer, buffer + sz);
}

ByteBuffer::ByteBuffer(uint8_t const* beg, uint8_t const* end)
{
    m_offset = 0;
    m_buffer.assign(beg, end);
}

ByteBuffer::ByteBuffer(string const& s)
{
    auto buffer = (uint8_t*)s.c_str();

    m_offset = 0;
    m_buffer.assign(buffer, buffer + s.length());
}

ByteBuffer::ByteBuffer(size_t size, uint8_t data)
{
    m_offset = 0;
    m_buffer.resize(size, data);
}

ByteBuffer::ByteBuffer(ByteBuffer const& rhs)
{
    if (this != &rhs)
    {
        m_offset = rhs.m_offset;
        m_buffer = rhs.m_buffer;
    }
}

ByteBuffer::ByteBuffer(ByteBuffer&& rhs)
{
    m_buffer.swap(rhs.m_buffer);
    m_offset = rhs.m_offset;
}

uint8_t& ByteBuffer::operator[](uint32_t index)
{
    if (index >= m_buffer.size())
        throw std::runtime_error("bad ByteBuffer index");

    return m_buffer[index];
}

uint8_t const& ByteBuffer::operator[](uint32_t index) const
{
    if (index >= m_buffer.size())
        throw std::runtime_error("bad ByteBuffer index");

    return m_buffer[index];
}

ByteBuffer& ByteBuffer::operator=(ByteBuffer const& rhs)
{
    if (this != &rhs)
    {
        ByteBuffer b(rhs);
        std::swap(m_offset,  b.m_offset);
        m_buffer.swap(b.m_buffer);
    }

    return *this;
}

// REMARK : offset을 비교해야 할지는 고려해봐야 한다.
bool ByteBuffer::operator==(ByteBuffer const& rhs) const
{
    if (this == &rhs)
        return true;

    return m_buffer == rhs.m_buffer;
}

bool ByteBuffer::has_remaining() const
{
    return m_offset < (int64_t)m_buffer.size();
}

int64_t ByteBuffer::remaining() const
{
    return m_buffer.size() - m_offset;
}

bool ByteBuffer::empty() const
{
    return m_buffer.empty();
}

ByteBuffer& ByteBuffer::flip()
{
    m_offset = 0;

    return *this;
}

int64_t ByteBuffer::offset() const
{
    return m_offset;
}

auto ByteBuffer::offset(int64_t o) const -> ByteBuffer const&
{
    m_offset = o;

    return *this;
}

ByteBuffer& ByteBuffer::reset()
{
    m_offset = 0;
    m_buffer.clear();

    return *this;
}

ByteBuffer& ByteBuffer::reset(uint32_t size, uint8_t value)
{
    m_offset = 0;
    m_buffer.clear();
    m_buffer.resize(size, value);

    return *this;
}

auto ByteBuffer::skip(uint32_t offset) const -> ByteBuffer const& 
{
    m_offset += offset;

    return *this;
}

ByteBuffer& ByteBuffer::unget(uint32_t no)
{
    m_offset -= no;

    return *this;
}

size_t ByteBuffer::reserve(size_t sz)
{
    m_buffer.reserve(sz);

    return m_buffer.capacity();
}

bool ByteBuffer::all_values_are(uint8_t value) const
{
    if (m_buffer.empty())
        return false;

    for (auto it=m_buffer.begin(); it!=m_buffer.end(); ++it)
        if (*it != value)
          return false;

    return true;
}

bool ByteBuffer::starts_with(string const& str) const
{
    auto len = str.length();
    string s((char*)&m_buffer[0], len);

    return str == s;
}

uint8_t ByteBuffer::last() const
{
    if (m_buffer.size() < 1)
        throw runtime_error("Buffer is empty()");

    return m_buffer[m_buffer.size() - 1];
}

auto ByteBuffer::first() const -> uint8_t
{
    if (m_buffer.size() < 1)
        throw runtime_error("Buffer is empty()");

    return m_buffer[0];
}

void ByteBuffer::reverse()
{
    std::reverse(m_buffer.begin(), m_buffer.end());
}

auto ByteBuffer::reverse_copy() const -> ByteBuffer
{
    ByteBuffer b(*this);
    b.reverse();

    return b;
}

auto ByteBuffer::last(uint32_t count) const -> ByteBuffer
{
    auto from = uint32_t(m_buffer.size()) - count;

    return slice(from, from+count);
}

auto ByteBuffer::first(uint32_t count) const -> ByteBuffer
{
    return slice(0, count);
}

auto ByteBuffer::take(uint32_t count)  const -> ByteBuffer
{
    if (count > m_buffer.size() - m_offset)
        throw std::runtime_error("ByteBuffer::take() : wrong size");

    auto res = slice(uint32_t(m_offset), uint32_t(m_offset + count));
    m_offset += count;

    return res;
}

ByteBuffer ByteBuffer::from_hexcode(string const& str, bool is_be)
{
    using namespace boost;
    ByteBuffer result;

    if (str.length() % 2 == 0 && all(str, is_xdigit()))
    {
        int size = int(str.length() / 2);
        for (int i=0; i<size; ++i)
        {
            auto in = is_be 
                ? str.substr((size - (i + 1)) * 2, 2)
                : str.substr(i * 2, 2);

            std::istringstream strm(in);
            uint32_t tmp; strm >> std::hex >> tmp;
            result.append((uint8_t)tmp);
        }
    }

    return result;
}

//
// REMAKR: 1. duplication get_hex_string
//         2. consider ByteBuffer Input instead of vector<uint8_t>
//
string ByteBuffer::to_hexcode(vector<uint8_t> const& code, bool is_be)
{
    buffer_t b;
    is_be ? std::reverse_copy(code.begin(), code.end(), back_inserter(b))
          :         std::copy(code.begin(), code.end(), back_inserter(b));

    ostringstream ss;
    for (size_t i=0; i<b.size(); i++)
      ss << setw(2) << setfill('0') << std::hex << (unsigned)b[i];

    return ss.str();
}

string ByteBuffer::to_hexcode(ByteBuffer const& code, bool is_be)
{
    auto c = const_cast<ByteBuffer&>(code);

    return ByteBuffer::to_hexcode(c.get_buffer(), is_be);
}

ByteBuffer& ByteBuffer::append(ByteBuffer& buffer)
{
    auto& b = buffer.get_buffer();
    m_buffer.insert(m_buffer.end(), b.begin(), b.end());

    return *this;
}

ByteBuffer& ByteBuffer::append(uint8_t* b, size_t sz)
{
    m_buffer.insert(m_buffer.end(), b, b + sz);

    return *this;
}

ByteBuffer& ByteBuffer::append(uint8_t b)
{
    m_buffer.push_back(b);

    return *this;
}

ByteBuffer ByteBuffer::slice(uint32_t from, uint32_t to) const
{
    if (from > to)
        throw std::runtime_error("ByteBuffer::slice() : wrong index");

    if (from == 0 && to == m_buffer.size())
        return *this;

    ByteBuffer result;
    auto& buffer = result.get_buffer();

    if (to > m_buffer.size())
        to = (uint32_t)m_buffer.size();

    buffer.assign(m_buffer.begin() + from, m_buffer.begin() + to);

    return result;
}

void ByteBuffer::load(buffer_t& buffer)
{
    m_buffer.swap(buffer);
}

uint8_t ByteBuffer::peek1_at(uint32_t offset, int start) const
{
    int64_t index = m_buffer.size() + 1;

    if (start == cur) index = m_offset + offset;  
    if (start == beg) index = offset;
    if (start == end) index = m_offset - offset;

    if (index >= (int64_t)m_buffer.size() || index < 0)
        throw std::runtime_error("ByteBuffer::peek1_at()");

    return m_buffer[size_type(index)];
}

int8_t ByteBuffer::get_int1() const
{ 
    int8_t res = (int8_t)m_buffer[size_type(m_offset)];
    m_offset++;

    return res;
}

uint8_t ByteBuffer::get_uint1() const
{
    uint8_t res = m_buffer[size_type(m_offset)];
    m_offset++;

    return res;
}

uint16_t ByteBuffer::get_uint2_net() const
{
    uint16_t res = *(uint16_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 2;

    return endian_swap_bytes<BIG_ENDIAN_ORDER, HOST_ENDIAN_ORDER>(res);
}

int16_t ByteBuffer::get_int2_net() const
{
    int16_t res = *(int16_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 2;

    return endian_swap_bytes<BIG_ENDIAN_ORDER, HOST_ENDIAN_ORDER>(res);
}

int32_t ByteBuffer::get_int4_net() const
{
    int32_t res = *(int32_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 4;

    return endian_swap_bytes<BIG_ENDIAN_ORDER, HOST_ENDIAN_ORDER>(res);
}

uint32_t ByteBuffer::get_uint4_net() const
{
    uint32_t res = *(uint32_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 4;

    return endian_swap_bytes<BIG_ENDIAN_ORDER, HOST_ENDIAN_ORDER>(res);
}

int16_t ByteBuffer::get_int2_le() const
{
    int16_t res = *(int16_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 2;

    return res;
}

uint16_t ByteBuffer::get_uint2_le() const
{
    uint16_t res = *(uint16_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 2;

    return res;
}

uint16_t ByteBuffer::get_uint2_be() const
{
    return get_int<uint16_t>(2);
}

int16_t ByteBuffer::get_int2_be() const
{
    return get_int<int16_t>(2);
}

uint32_t ByteBuffer::get_uint3_be() const
{
    return get_int<uint32_t>(3);
}

int32_t ByteBuffer::get_int4_be() const
{
    return int32_t(get_int<uint32_t>(4));
}

int32_t ByteBuffer::get_int4_le() const
{
    int32_t res = *(int32_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 4;

    return res;
}

uint32_t ByteBuffer::get_uint4_be() const
{
    return get_int<uint32_t>(4);
}

uint32_t ByteBuffer::get_uint4_le() const
{
    uint32_t res = *(uint32_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 4;

    return res;
}

uint64_t ByteBuffer::get_uint8_be() const
{
    return get_int<uint64_t>(8);    
}

int64_t ByteBuffer::get_int8_be() const
{
    return get_int<int64_t>(8);
}

uint64_t ByteBuffer::get_uint8_le() const
{
    uint64_t res = *(uint64_t*)(uint8_t*)&m_buffer[size_t(m_offset)];
    m_offset += 8;

    return res;
}

uint8_t* ByteBuffer::get_binary(uint32_t size) const
{
    if (m_offset + size > int64_t(m_buffer.size()))
        throw std::runtime_error("ByteBuffer::get_binary(size)");

    uint8_t* res = (uint8_t*)&m_buffer[size_type(m_offset)];
    m_offset += size;

    return res;
}

string ByteBuffer::get_hex_string(uint32_t size) const
{
    std::string result;

    for (uint32_t i=0; i<size; i++)
    {
        char b[10] = { 0 };
        sprintf(b, "%02x", m_buffer[size_type(m_offset + i)]);
        result += std::string(b);
    }

    m_offset += size;

    return result;
}

string ByteBuffer::get_string() const
{
    int64_t offset = m_offset;
    for (; (offset < m_buffer.size()) && (m_buffer[size_type(offset)] != 0);
        ++offset);
    int64_t size = offset - m_offset; 

    std::string result = get_string(size_t(size));
    m_offset += 1; // consume NULL

    return result;
}

string ByteBuffer::get_string(size_t size) const
{
    std::string result((char*)&m_buffer[size_type(m_offset)], size);
    m_offset += size;  

    return result;
}

string ByteBuffer::to_s() const
{
    std::string result((char*)&m_buffer[0], m_buffer.size());

    return result;
}

char const* ByteBuffer::c_str() const
{
    char const* res = (char const*)&m_buffer[size_type(m_offset)];
    for (; m_buffer[size_type(m_offset)] != 0; ++m_offset);
    m_offset++;

    return res;
}

ByteBuffer& ByteBuffer::set_uint1(uint8_t src)
{
    m_buffer.push_back(src);
    m_offset++;

    return *this;
}

ByteBuffer& ByteBuffer::set_uint2_net(uint16_t src)
{
    src = endian_swap_bytes<HOST_ENDIAN_ORDER, BIG_ENDIAN_ORDER>(src);
    uint8_t no[2] = { 0 };
    memcpy((void*)no, (void*)&src, 2);

    for (size_t i=0; i<2; i++) m_buffer.push_back(no[i]);
    m_offset += 2;

    return *this;
}

ByteBuffer& ByteBuffer::set_uint4_net(uint32_t src)
{
    src = endian_swap_bytes<HOST_ENDIAN_ORDER, BIG_ENDIAN_ORDER>(src);
    uint8_t no[4] = { 0 };
    memcpy((void*)no, (void*)&src, 4);

    for (size_t i=0; i<4; i++) m_buffer.push_back(no[i]);
    m_offset += 4;

    return *this;
}

ByteBuffer& ByteBuffer::set_string(char const* src)
{
    if (src != NULL)
    {
        size_t len = strlen(src);
        for (size_t i=0; i<len; i++) m_buffer.push_back(src[i]);
        m_offset += len;
    }

    m_buffer.push_back(0);
    m_offset++;

    return *this;
}

ByteBuffer& ByteBuffer::set_binary(uint8_t* src, uint32_t len)
{
    for (size_t i=0; i<len; i++) m_buffer.push_back(src[i]);
    m_offset += len;

    return *this;
}

ByteBuffer& ByteBuffer::set_uint2_le(uint16_t src)
{
    src = endian_swap_bytes<HOST_ENDIAN_ORDER, LITTLE_ENDIAN_ORDER>(src);
    uint8_t no[2] = { 0 };
    memcpy((void*)no, (void*)&src, 2);

    for (size_t i=0; i<2; i++) m_buffer.push_back(no[i]);
    m_offset += 2;

    return *this;
}

ByteBuffer& ByteBuffer::set_uint4_le(uint32_t src)
{
    src = endian_swap_bytes<HOST_ENDIAN_ORDER, LITTLE_ENDIAN_ORDER>(src);
    uint8_t no[4] = { 0 };
    memcpy((void*)no, (void*)&src, 4);

    for (size_t i=0; i<4; i++) m_buffer.push_back(no[i]);
    m_offset += 4;

    return *this;
}

ByteBuffer::buffer_t& ByteBuffer::get_buffer()
{ 
    return m_buffer; 
}

ByteBuffer::buffer_t& ByteBuffer::get_buffer(uint32_t size)
{
    m_buffer.resize(size);

    return m_buffer;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
