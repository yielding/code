#pragma once

#include "BPlistRepr.h"
#include "ByteBuffer.h"

namespace utility { namespace parser {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using utility::hex::ByteBuffer;

class PropertyList
{
public:
    enum { TRAILER_SIZE  = 26 }; // sizeof(uint8_t) * 2 + sizeof(uint64_t)*3;

public:
    typedef std::vector<uint8_t> buffer_type;

public:
    PropertyList() : m_is_open(false) {}
    PropertyList(std::string const& fname);
    PropertyList(ByteBuffer const& buffer);
    PropertyList(CFType t);
    ~PropertyList();

public:
    void close();
    bool open(std::string const& fname);
    bool load(char* buffer, size_t size);

public:
    PropertyList&  set(CFType const v);
    static int     type(CFType const t);
    int            type();

    std::string    to_xml() const;
    ByteBuffer     to_bin();

public:
    CFType         handle();
    CFArray        as_array();
    CFDictionary   as_dictionary();

private:
    bool           init();
    void           get_root_object();
    CFType         read_object_at(int64_t pos);
    CFType         read_object_at_ref(int64_t ref);
    CFType         read_object();
    int64_t        get_length(uint8_t nibble);
    bool           read_file(std::string const& path);

private:
    uint8_t m_offset_size;
    uint8_t m_offset_ref_size;
    int64_t m_object_count;
    int64_t m_top_object;
    int64_t m_table_offset;
    std::vector<int64_t> m_offset_table;

private:
    ByteBuffer m_buffer;
    CFType m_value;
    bool m_is_open;
};
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
