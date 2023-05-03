#pragma once

#include <vector>
#include <string>
#include <stdint.h>
#include <stdexcept>

namespace utility { namespace hex {
////////////////////////////////////////////////////////////////////////////////
//
//
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
    // This constructor is for network I/O
    ByteBuffer(size_t size=0);

    // This constructor is for hex conversion
    ByteBuffer(uint8_t* buffer, size_t s);

    ByteBuffer(ByteBuffer const& rhs);
    ByteBuffer(ByteBuffer&& rhs);
    ~ByteBuffer() {}

    ByteBuffer& operator=(ByteBuffer const& rhs);

    bool operator == (ByteBuffer const& rhs) const;

public:  // operators
    operator uint8_t*()        { return m_buffer.data(); }
    operator char const*()     { return (char const*)m_buffer.data(); }
    operator void const*()     { return (void const*)m_buffer.data(); }

public:  // query
    bool    has_remaining();
    int64_t remaining();
    bool    empty();
    int64_t size()             { return int64_t(m_buffer.size());     }
    int64_t capacity()         { return int64_t(m_buffer.capacity()); }
    size_t  reserve(size_t);

public:
    static ByteBuffer  from_hexcode(std::string const&, bool=false);
    static std::string to_hexcode(std::vector<uint8_t> const&, bool=false);

public:
    int64_t     offset();
    ByteBuffer& offset(uint32_t o);
    ByteBuffer& reset();
    ByteBuffer& flip();

    buffer_t&   get_buffer();
    buffer_t&   get_buffer(uint32_t size);

    ByteBuffer& skip(uint32_t offset);
    ByteBuffer& unget(uint32_t offset);
    ByteBuffer& append(ByteBuffer& b);
    ByteBuffer& append(uint8_t* b, size_t sz);
    ByteBuffer& append(uint8_t b);
    ByteBuffer  slice(uint32_t from, uint32_t to);

    void        load(buffer_t& buffer);
    uint8_t     peek1_at(uint32_t offset, int start=cur);

    // network I/O interface
    uint16_t    get_uint2_net();
    int16_t     get_int2_net();
    int32_t     get_int4_net();
    uint32_t    get_uint4_net();

    ByteBuffer& set_uint2_net(uint16_t);
    ByteBuffer& set_uint4_net(uint32_t);

    ByteBuffer& set_uint1(uint8_t);
    ByteBuffer& set_string(char const* src);
    ByteBuffer& set_binary(uint8_t* src, uint32_t size);

    int8_t   get_int1();
    uint8_t  get_uint1();
    int16_t  get_int2_be();
    uint16_t get_uint2_be();
    int16_t  get_int2_le();
    uint16_t get_uint2_le();
    uint32_t get_uint3_be();
    int32_t  get_int4_be();
    int32_t  get_int4_le();
    uint32_t get_uint4_be();
    uint32_t get_uint4_le();
    int64_t  get_int8_be();

    uint64_t get_uint8_le();
    uint64_t get_uint8_be();
    uint8_t* get_binary(uint32_t size);

    std::string get_hex_string(uint32_t size);
    std::string get_string();
    std::string get_string(size_t size);
    char const* c_str();

public:
    template<typename IntType>
    IntType get_int(size_t byte_count)
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
    uint32_t m_offset;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
