#include "byte_buffer2.h"

#include <climits>
#include <utility>
#include <iostream>
#include <cctype>
#include <algorithm>
#include <sstream>
#include <exception>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {

template <typename T>
T swap_endian(T u)
{
  static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");

  union
  {
    T u;
    uint8_t u8[sizeof(T)];
  } source, dest;

  source.u = u;

  for (auto k=0; k<sizeof(T); k++)
    dest.u8[k] = source.u8[sizeof(T) - k - 1];

  return dest.u;
}

}

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer2 is for shallow version of buffer
//
////////////////////////////////////////////////////////////////////////////////
ByteBuffer2::ByteBuffer2(uint8_t* data, int offset, int count, bool owner)
{
  m_data   = data;
  m_offset = offset;
  m_begin  = offset;
  m_count  = count;
  m_limit  = m_begin + count;
  m_owner  = owner;
}

ByteBuffer2::ByteBuffer2(uint8_t* data, int count)
  : ByteBuffer2(data, 0, count, false) 
{}

ByteBuffer2::ByteBuffer2()
  : ByteBuffer2(nullptr, 0, 0) 
{}

ByteBuffer2::ByteBuffer2(string && src)
  : ByteBuffer2((uint8_t*)src.data(), 0, (int)src.length())
{}

ByteBuffer2::ByteBuffer2(initializer_list<uint8_t> l)
{
  reset(l);
}

ByteBuffer2::ByteBuffer2(ByteBuffer2&& rhs)
{
  m_data   = rhs.m_data;
  m_offset = rhs.m_offset;
  m_begin  = rhs.m_begin;
  m_count  = rhs.m_count;
  m_limit  = rhs.m_limit;
  m_owner  = rhs.m_owner;
  
  rhs.m_data  = nullptr;
  rhs.m_owner = false;
}

ByteBuffer2::~ByteBuffer2()
{
  if (m_owner)
  {
    delete [] m_data;
    m_data = nullptr;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer2 is for shallow version of buffer
//
////////////////////////////////////////////////////////////////////////////////
auto ByteBuffer2::operator[](uint32_t index) -> uint8_t
{
  check_offset(index);
  return m_data[m_begin + index];
}

auto ByteBuffer2::operator[](uint32_t index) const -> uint8_t const
{
  check_offset(index);

  return m_data[m_begin + index];
}

auto ByteBuffer2::get_int8() const -> int8_t
{
  check_offset(1);

  return (int8_t)m_data[m_offset++];
}

auto ByteBuffer2::get_uint8() const -> uint8_t
{
  check_offset(1);

  return m_data[m_offset++];
}

auto ByteBuffer2::get_int16_be() const -> int16_t
{
  check_offset(2);

  auto res = *(int16_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 2;

  return swap_endian<int16_t>(res);
}

auto ByteBuffer2::get_int16_le() const -> int16_t
{
  check_offset(2);

  auto res = *(int16_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 2;

  return res;
}

auto ByteBuffer2::get_uint16_be() const -> uint16_t
{
  check_offset(2);

  auto res = *(uint16_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 2;

  return swap_endian<uint16_t>(res);
}

auto ByteBuffer2::get_uint16_le() const -> uint16_t
{
  check_offset(2);

  auto res = *(uint16_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 2;

  return res;
}

auto ByteBuffer2::get_int24_be() const -> int32_t
{
  check_offset(3);

  auto result = (int32_t)leading_byte(m_data[m_offset]);

  for (int i=m_offset; i<m_offset+3; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 3;

  return result;
}

auto ByteBuffer2::get_int24_le() const -> int32_t
{
  return 0;
}

auto ByteBuffer2::get_uint24_be() const -> uint32_t
{
  check_offset(3);

  auto result = (uint32_t)0;

  for (int i=m_offset; i<m_offset+3; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 3;

  return result;
}

auto ByteBuffer2::get_uint24_le() const -> uint32_t
{
  return 0;
}

auto ByteBuffer2::get_int32_be() const -> int32_t
{
  check_offset(4);

  auto res = *(int32_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 4;

  return swap_endian<int32_t>(res);
}

auto ByteBuffer2::get_int32_le() const -> int32_t
{
  check_offset(4);

  auto res = *(int32_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 4;

  return res;
}

auto ByteBuffer2::get_uint32_be() const -> uint32_t
{
  check_offset(4);

  auto res = *(uint32_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 4;

  return swap_endian<uint32_t>(res);
}

auto ByteBuffer2::get_uint32_le() const -> uint32_t
{
  check_offset(4);

  auto res = *(uint32_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 4;

  return res;
}

auto ByteBuffer2::get_int40_be() const -> int64_t
{
  check_offset(5);

  auto sign = (int64_t)leading_byte(m_data[m_offset]);
  auto result = (sign << 16) | (sign << 8) | sign;

  for (int i=m_offset; i<m_offset+5; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 5;

  return result;
}

auto ByteBuffer2::get_int40_le()  const -> int64_t
{
  check_offset(5);
  auto sign = (int64_t)leading_byte(m_data[m_offset+4]);
  auto result = (sign << 16) | (sign << 8) | sign;

  for (int i=m_offset+4; i>=m_offset; i--)
    result = (result << 8) + m_data[i]; 

  return result;
}

auto ByteBuffer2::get_uint40_be() const -> uint64_t
{
  check_offset(5);

  auto result = (uint64_t)0;

  for (int i=m_offset; i<m_offset+5; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 5;

  return result;
}

auto ByteBuffer2::get_uint40_le() const -> uint64_t
{
  check_offset(5);
  auto result = (uint64_t)0;
  for (int i=m_offset+4; i>=m_offset; i--)
    result = (result << 8) + m_data[i]; 

  return result;
}

auto ByteBuffer2::get_int48_be() const -> int64_t
{
  check_offset(6);

  auto sign = (int64_t)leading_byte(m_data[m_offset]);
  auto result = (sign << 8) | sign;

  for (int i=m_offset; i<m_offset+6; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 6;

  return result;
}

auto ByteBuffer2::get_int48_le() const -> int64_t
{
  check_offset(6);

  auto sign = (int64_t)leading_byte(m_data[m_offset+5]);
  auto result = (sign << 8) | sign;
  for (int i=m_offset+5; i>=m_offset; i--)
    result = (result << 8) + m_data[i]; 

  return result;
}

auto ByteBuffer2::get_uint48_be() const -> uint64_t
{
  check_offset(6);

  auto result = (uint64_t)0;

  for (int i=m_offset; i<m_offset+6; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 6;

  return result;
}

auto ByteBuffer2::get_uint48_le() const -> uint64_t
{
  check_offset(6);

  auto result = (uint64_t)0;
  for (int i=m_offset+5; i>=m_offset; i--)
    result = (result << 8) + m_data[i]; 

  return result;
}

auto ByteBuffer2::get_int56_be() const -> int64_t
{
  check_offset(7);

  auto result = (int64_t)leading_byte(m_data[m_offset]);

  for (int i=m_offset; i<m_offset+7; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 7;

  return result;
}

auto ByteBuffer2::get_int56_le() const -> int64_t
{
  check_offset(7);

  auto result = (int64_t)leading_byte(m_data[m_offset+6]);
  for (int i=m_offset+6; i>=m_offset; i--)
    result = (result << 8) + m_data[i]; 

  return result;
}

auto ByteBuffer2::get_uint56_be() const -> uint64_t
{
  check_offset(7);

  auto result = (uint64_t)0;

  for (int i=m_offset; i<m_offset+7; i++)
    result = (result << 8) + m_data[i]; 

  m_offset += 7;

  return result;
}

auto ByteBuffer2::get_uint56_le() const -> uint64_t
{
  check_offset(7);

  auto result = (uint64_t)0;
  for (int i=m_offset+6; i>=m_offset; i--)
    result = (result << 8) + m_data[i]; 

  return result;
}

auto ByteBuffer2::get_int64_be() const -> int64_t
{
  check_offset(8);

  auto res = *(int64_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 8;

  return swap_endian<int64_t>(res);
}

auto ByteBuffer2::get_int64_le() const -> int64_t
{
  check_offset(8);

  auto res = *(int64_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 8;

  return res;
}

auto ByteBuffer2::get_uint64_be() const -> uint64_t
{
  check_offset(8);

  auto res = *(uint64_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 8;

  return swap_endian<uint64_t>(res);
}

auto ByteBuffer2::get_uint64_le() const -> uint64_t
{
  check_offset(8);

  auto res = *(uint64_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 8;

  return res;
}

auto ByteBuffer2::get_double() const -> double
{
  auto res = get_int64_be();

  return *(double *)&res;
}

auto ByteBuffer2::get_bytes(int size) const -> uint8_t*
{
  check_offset(size);
  
  auto result = &m_data[m_offset];
  m_offset += size;

  return result;
}

auto ByteBuffer2::get_hex_string(int size) -> const string
{
  constexpr char hmap[] =
    { '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

  string result(size * 2, ' ');

  for (int i=0; i<size; i++)
  {
    result[2*i + 0] = hmap[(m_data[m_offset + i] & 0xf0) >> 4];
    result[2*i + 1] = hmap[(m_data[m_offset + i] & 0x0f)];
  }

  m_offset += size;

  return result;
}

auto ByteBuffer2::get_varint() const -> int64_t
{
  int size = 0; return get_varint_with_size(&size);
}

auto ByteBuffer2::get_varint2() const -> std::pair<int64_t, int>
{
  int size = 0; 
  auto result = get_varint_with_size(&size);

  return make_pair(result, size);
}

auto ByteBuffer2::get_varint_with_size(int* size) const -> int64_t
{
  auto complete = false;
  auto value_size = 0;
  auto value = 0L;

  while (value_size < 9 && !complete && m_offset + value_size < m_limit)
  {
    auto val = int64_t(m_data[m_offset + value_size]);
    if ((val & 0b10000000) == 0b10000000 && value_size < 8)
    {
      value = (value << 7) | (val & 0b01111111);
    } 
    else if ((val & 0b10000000) == 0b10000000 && value_size == 8) { 
      value = (value << 8) | val;
      complete = true;
    } 
    else 
    {
      value = (value << 7) | (val & 0b01111111);
      complete = true;
    }   

    value_size += 1;
  }

  if (!complete)
    throw logic_error("invalid varint");

  m_offset += value_size;
  *size = value_size;

  return value;
}

auto ByteBuffer2::has_remaining() const -> bool
{
  return m_offset < m_limit;
}

auto ByteBuffer2::remained_size() const -> int
{
  return m_limit - m_offset;
}

auto ByteBuffer2::advance(int count) -> ByteBuffer2&
{
  m_offset += count;

  return *this;
}

auto ByteBuffer2::skip(int count) -> ByteBuffer2&
{
  m_offset += count;

  return *this;
}

auto ByteBuffer2::take(int amount) const -> ByteBuffer2
{
  ByteBuffer2 subrange(m_data, m_offset, amount);
  m_offset += amount;

  return subrange;
}

auto ByteBuffer2::slice(int from, int count) -> ByteBuffer2
{
  return ByteBuffer2(m_data, from, count);
}

auto ByteBuffer2::first() const -> uint8_t
{
  check_offset(1);

  return m_data[0];
}

auto ByteBuffer2::first(int amount) const -> ByteBuffer2
{
  check_offset(amount);

  return ByteBuffer2(m_data, m_begin, amount);
}

auto ByteBuffer2::last() const -> uint8_t
{
  check_offset(1);

  return m_data[m_limit - 1];
}

auto ByteBuffer2::last(int amount) const -> ByteBuffer2
{
  ByteBuffer2 subrange(m_data, m_limit - amount, amount);

  return subrange;
}

auto ByteBuffer2::starts_with(string const& str) const -> bool
{
  if (str.length() > m_count)
    throw out_of_range("array out of index");
    
  auto len = str.length();
  string s((char*)&m_data[0], len);

  return str == s;
}

auto ByteBuffer2::reset() -> ByteBuffer2&
{
  m_offset = m_begin;

  return *this;
}

auto ByteBuffer2::reset(initializer_list<uint8_t> l) -> void
{
  m_data   = (uint8_t *)&*l.begin();
  m_offset = 0;
  m_begin  = 0;
  m_count  = (int)l.size();
  m_limit  = m_count;
  m_owner  = false;
}

auto ByteBuffer2::get_ascii() const -> string
{
  auto offset = m_offset;

  for (; offset < m_limit && m_data[offset] != 0; ++offset)
    ;

  auto size = offset - m_offset;

  string result((char*)&m_data[m_offset], size);

  m_offset += size + 1;

  return result;
}

auto ByteBuffer2::from_hexcode(string const& s, bool is_be) -> ByteBuffer2
{
  auto all_hex = all_of(s.cbegin(), s.cend(), [](int c) { return isxdigit(c) != 0; });

  if (s.length() % 2 == 0 && all_hex)
  {
    auto size = int(s.length() / 2);
    auto data = new uint8_t[size];

    for (int i=0; i<size; ++i)
    {
      auto in = is_be 
        ? s.substr((size - (i + 1)) * 2, 2)
        : s.substr(i * 2, 2);

      istringstream strm(in);
      uint16_t val; strm >> hex >> val;
      data[i] = (uint8_t)val;
    }

    ByteBuffer2 result(data, 0, size, true);
    return move(result);
  }
  
  return ByteBuffer2();
}

auto ByteBuffer2::to_s(int from, int to) const -> std::string
{
  if (from == -1) from = m_offset;
  if (to == -1) to = m_limit;

  return string((char*)&m_data[from], to - from);
}

auto ByteBuffer2::check_offset(int count) const -> void
{
  if (m_offset + count > m_limit) 
    throw out_of_range("array out of index");
}

auto ByteBuffer2::leading_byte(uint8_t b) const -> uint8_t 
{
  return (b & 0x80) == 0 ? 0 : 0xff;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
