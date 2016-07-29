#include "byte_buffer2.h"

#include <climits>

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

    for (size_t k = 0; k < sizeof(T); k++)
        dest.u8[k] = source.u8[sizeof(T) - k - 1];

    return dest.u;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer2 is for shallow version of buffer
//
////////////////////////////////////////////////////////////////////////////////
ByteBuffer2::ByteBuffer2(uint8_t* data, int offset, int count)
{
  m_data = data;

  m_offset = offset;
  m_begin  = offset;
  m_limit  = m_begin + count;
}

auto ByteBuffer2::get_int8() const -> int8_t
{
  return (int8_t)m_data[m_offset++];
}

auto ByteBuffer2::get_uint8() const -> uint8_t
{
  return m_data[m_offset++];
}

auto ByteBuffer2::get_int16_be() const -> int16_t
{
  auto res = *(int16_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 2;

  return swap_endian<int16_t>(res);
}

auto ByteBuffer2::get_int16_le() const -> int16_t
{
  auto res = *(int16_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 2;

  return res;
}

auto ByteBuffer2::get_int32_be() const -> int32_t
{
  auto res = *(int32_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 4;

  return swap_endian<int32_t>(res);
}

auto ByteBuffer2::get_int32_le() const -> int32_t
{
  auto res = *(int32_t*)(uint8_t*)&m_data[m_offset];
  m_offset += 4;

  return res;
}

auto ByteBuffer2::has_remaining() const -> bool
{
  return m_offset < m_limit;
}

auto ByteBuffer2::reset() -> ByteBuffer2&
{
  m_offset = m_begin;

  return *this;
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
