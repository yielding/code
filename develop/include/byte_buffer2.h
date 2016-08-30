#ifndef BYTE_BUFFER_H
#define BYTE_BUFFER_H

#include <cstdint>
#include <string>
#include <initializer_list>

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer2 is basically, for shallow version of buffer.
//
// But, It can contain buffer memory
//
////////////////////////////////////////////////////////////////////////////////
class ByteBuffer2
{
public:
  ByteBuffer2();
  ByteBuffer2(uint8_t* data, int offset, int count, bool owner=false);
  ByteBuffer2(uint8_t* data, int count);
  ByteBuffer2(std::string && src);
  ByteBuffer2(std::initializer_list<uint8_t> l);
  ByteBuffer2(ByteBuffer2 &&);

 ~ByteBuffer2();

public:
  auto get_ascii() const -> std::string;
  auto to_s(int from=-1, int to=-1) const -> std::string;

  auto get_int8()  const -> int8_t;
  auto get_uint8() const -> uint8_t;

  auto get_int16_be()  const -> int16_t;
  auto get_int16_le()  const -> int16_t;
  auto get_uint16_be() const -> uint16_t;
  auto get_uint16_le() const -> uint16_t;

  auto get_int24_be()  const -> int32_t;
  auto get_int24_le()  const -> int32_t;
  auto get_uint24_be() const -> uint32_t;
  auto get_uint24_le() const -> uint32_t;

  auto get_int32_be()  const -> int32_t;
  auto get_int32_le()  const -> int32_t;
  auto get_uint32_be() const -> uint32_t;
  auto get_uint32_le() const -> uint32_t;

  auto get_int40_be()  const -> int64_t;
  auto get_int40_le()  const -> int64_t;
  auto get_uint40_be() const -> uint64_t;
  auto get_uint40_le() const -> uint64_t;

  auto get_int48_be()  const -> int64_t;
  auto get_int48_le()  const -> int64_t;
  auto get_uint48_be() const -> uint64_t;
  auto get_uint48_le() const -> uint64_t;

  auto get_int56_be()  const -> int64_t;
  auto get_int56_le()  const -> int64_t;
  auto get_uint56_be() const -> uint64_t;
  auto get_uint56_le() const -> uint64_t;

  auto get_int64_be()  const -> int64_t;
  auto get_int64_le()  const -> int64_t;
  auto get_uint64_be() const -> uint64_t;
  auto get_uint64_le() const -> uint64_t;

  auto get_double()    const -> double;

  auto get_bytes(int)  const -> uint8_t*;
  auto get_hex_string(int) -> const std::string;

  auto get_varint()    const -> int64_t;
  auto get_varint2()   const -> std::pair<int64_t, int>;
  auto get_varint_with_size(int*) const -> int64_t;

  auto has_remaining() const -> bool;
  auto remained_size() const -> int;

  auto offset(int off)       { m_offset = off;  }
  auto offset() const -> int { return m_offset; }
  auto size() const -> int   { return m_count;  }

  auto advance(int) -> ByteBuffer2&;
  auto skip(int) -> ByteBuffer2&;
  auto take(int) const -> ByteBuffer2;

  auto slice(int from, int count) -> ByteBuffer2;

  auto first() const -> uint8_t;
  auto first(int) const -> ByteBuffer2;

  auto last() const -> uint8_t;
  auto last(int) const -> ByteBuffer2;

  auto starts_with(std::string const& str) const -> bool;

  auto reset() -> ByteBuffer2&;
  auto reset(std::initializer_list<uint8_t> l) -> void;

public:
  auto operator[](uint32_t index) -> uint8_t;
  auto operator[](uint32_t index) const -> uint8_t const;

public:
  static auto from_hexcode(std::string const& s, bool is_be=false) -> ByteBuffer2;

private:
  auto check_offset(int) const -> void;
  auto leading_byte(uint8_t) const -> uint8_t;

private:
  mutable int m_offset;

  int m_count;
  int m_limit;
  int m_begin;

  uint8_t* m_data;
  bool m_owner;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
