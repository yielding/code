#ifndef BYTE_BUFFER_H
#define BYTE_BUFFER_H

#include <cstdint>
#include <string>

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer2 is for shallow version of buffer
//
////////////////////////////////////////////////////////////////////////////////
class ByteBuffer2
{
public:
  ByteBuffer2(uint8_t* data, int offset, int count);

public:
  auto get_ascii() const -> std::string;

  auto get_int8() const -> int8_t;
  auto get_uint8() const -> uint8_t;

  auto get_int16_be() const -> int16_t;
  auto get_int16_le() const -> int16_t;

  auto get_int32_be() const -> int32_t;
  auto get_int32_le() const -> int32_t;

  auto has_remaining() const -> bool;
  auto offset() const -> int { return m_offset; }

  auto reset() -> ByteBuffer2&;

private:
  mutable int m_offset;

  int m_count;
  int m_limit;
  int m_begin;

  uint8_t* m_data;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
