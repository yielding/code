#pragma once

#include "bplist_repr.hpp"
#include "byte_buffer.hpp"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace util::parser 
{
  using namespace std;
  using byte_buffer = sys::io::byte_buffer;

  class PropertyList
  {
  public:
    enum { TRAILER_SIZE  = 26 }; // sizeof(uint8_t) * 2 + sizeof(uint64_t)*3;

  public:
    typedef vector<uint8_t> buffer_type;

  public:
    PropertyList() : m_is_open(false) {}
    explicit PropertyList(string const& fname);
    explicit PropertyList(byte_buffer const& buffer);
    explicit PropertyList(CFType t);
    ~PropertyList();

  public:
    auto close() -> void;
    auto open(string const& fname) -> bool;
    auto load(char* buffer, size_t size) -> bool;

  public:
    auto offset_size() const { return m_offset_size; }
    auto offset_ref_size() const { return m_offset_ref_size; }
    auto object_count() const { return m_object_count; }
    auto top_object_start() const { return m_top_object; }
    auto offset_table_start() const { return m_offset_table_start; }

  public:
    auto set(CFType v) -> PropertyList&;
    auto type() -> int;
    static auto type(CFType t) -> int;

    auto to_xml() const -> string;
    auto to_bin() -> byte_buffer;

  public:
    auto handle() -> CFType;
    auto as_array() -> CFArray;
    auto as_dictionary() -> CFDictionary;

  private:
    auto init() -> bool;
    auto get_root_object() -> CFType;
    auto read_object_at(int64_t pos) -> CFType;
    auto read_object_at_ref(int64_t ref) -> CFType;
    auto read_object() -> CFType;
    auto get_length(uint8_t nibble) -> int64_t;
    auto read_file(string const& path) -> bool;

  private:
    uint8_t m_offset_size{};
    uint8_t m_offset_ref_size{};
    int64_t m_object_count{};
    int64_t m_top_object{};
    int64_t m_offset_table_start{};
    vector<int64_t> m_offset_table;

  private:
    byte_buffer m_buffer;
    CFType m_value;
    bool m_is_open;
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
