#pragma once

#include <vector>
#include <string>
#include <stdint.h>
#include <stdexcept>

namespace utility { namespace hex {
////////////////////////////////////////////////////////////////////////////////
//
// NOTICE m_offset is mutable!!!
//
////////////////////////////////////////////////////////////////////////////////
class ByteBuffer
{
public:
    typedef std::vector<uint8_t> buffer_t;
    typedef buffer_t::size_type size_type;

public:
    enum { beg = 0, cur = 1, end = 2 };

public:
    // for network I/O
    ByteBuffer(size_t size=0);

    ByteBuffer(uint8_t* buffer, size_t s);
    ByteBuffer(uint8_t const* beg, uint8_t const* end);
    
    ByteBuffer(std::string const&);
    ByteBuffer(size_t size, uint8_t data);

    ByteBuffer(ByteBuffer const& rhs);
    ByteBuffer(ByteBuffer&& rhs);
    ~ByteBuffer() {}

    ByteBuffer& operator=(ByteBuffer const& rhs);

    bool operator == (ByteBuffer const& rhs) const;

public:  // operators
    operator uint8_t*()        { return m_buffer.data(); }
    operator char*()           { return (char*)&m_buffer[0]; }
    operator char const*()     { return (char const*)m_buffer.data(); }
    operator void const*()     { return (void const*)m_buffer.data(); }

public:
    std::string to_str() const { return std::string((char const*)m_buffer.data(), m_buffer.size()); }
  
public:  // query
    bool    has_remaining() const;
    int64_t remaining()     const;
    bool    empty()         const;
    size_t  size()          const { return m_buffer.size();           }
    size_t  capacity()      const { return m_buffer.capacity();       }
    size_t  reserve(size_t);

    bool all_values_are(uint8_t value)       const;
    bool starts_with(std::string const& str) const;
    auto last()                const -> uint8_t;
    auto last(uint32_t count)  const -> ByteBuffer;
    auto first()               const -> uint8_t;
    auto first(uint32_t count) const -> ByteBuffer;
    auto take(uint32_t count)  const -> ByteBuffer;

    void reverse();
    auto reverse_copy() const -> ByteBuffer;

public:
    static ByteBuffer  from_hexcode(std::string const&, bool=false);
    static std::string to_hexcode(std::vector<uint8_t> const&, bool=false);
    static std::string to_hexcode(ByteBuffer const&, bool=false);

public:
    uint8_t&       operator[](uint32_t index);
    uint8_t const& operator[](uint32_t index) const;

public: // network I/O interface
    auto flip() -> ByteBuffer&;

    auto get_uint2_net() const -> uint16_t;
    auto get_int2_net()  const -> int16_t;
    auto get_int4_net()  const -> int32_t;
    auto get_uint4_net() const -> uint32_t;

    auto set_uint2_net(uint16_t) -> ByteBuffer&;
    auto set_uint4_net(uint32_t) -> ByteBuffer&;

public:
    auto offset() const -> int64_t;
    auto offset(int64_t o) const -> ByteBuffer const&;
    auto reset() -> ByteBuffer&;
    auto reset(uint32_t size, uint8_t value) -> ByteBuffer&;

    auto get_buffer() -> buffer_t&;
    auto get_buffer(uint32_t size) -> buffer_t&;

    auto skip(uint32_t offset) const -> ByteBuffer const&;
    auto unget(uint32_t offset) -> ByteBuffer&;
    auto append(ByteBuffer& b) -> ByteBuffer&;
    auto append(uint8_t* b, size_t sz) -> ByteBuffer&;
    auto append(uint8_t b) -> ByteBuffer&;
    auto slice(uint32_t from, uint32_t to) const -> ByteBuffer;

    auto load(buffer_t& buffer) -> void;
    auto peek1_at(uint32_t offset, int start=cur) const -> uint8_t;

    auto set_uint1(uint8_t) -> ByteBuffer&;
    auto set_string(char const* src) -> ByteBuffer&;
    auto set_binary(uint8_t* src, uint32_t size) -> ByteBuffer&;

    auto set_uint4_le(uint32_t) -> ByteBuffer&;
    auto set_uint2_le(uint16_t) -> ByteBuffer&;
    auto set_varint(uint64_t)   -> ByteBuffer&;

    auto get_int1()     const -> int8_t;
    auto get_uint1()    const -> uint8_t;
    auto get_int2_be()  const -> int16_t;
    auto get_uint2_be() const -> uint16_t;
    auto get_int2_le()  const -> int16_t;
    auto get_uint2_le() const -> uint16_t;
    auto get_uint3_be() const -> uint32_t;
    auto get_int4_be()  const -> int32_t;
    auto get_int4_le()  const -> int32_t;
    auto get_uint4_be() const -> uint32_t;
    auto get_uint4_le() const -> uint32_t;
    auto get_int8_be()  const -> int64_t;

    auto get_uint8_le() const -> uint64_t;
    auto get_uint8_be() const -> uint64_t;
    auto get_varint()   const -> uint64_t;
    auto get_varint(uint8_t& sz) const -> uint64_t;

    auto get_binary(uint32_t size) const -> uint8_t*;

    auto get_hex_string(uint32_t size) const -> std::string;
    auto get_string() const -> std::string;
    auto get_string(size_t size) const -> std::string;

    auto to_s()  const -> std::string;
    auto c_str() const -> char const*;

public:
    template<typename IntType>
    IntType get_int(size_t byte_count) const
    {
        IntType res = 0;
        for (size_t i=0; i<byte_count; ++i)
        {
            uint8_t b = get_uint1();
            res |= IntType(b) << ((byte_count - 1 - i) * 8);
        }
        
        return res;
    }

protected:
    buffer_t m_buffer;
    mutable int64_t m_offset;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} // namespace hex
} // namespace utility
