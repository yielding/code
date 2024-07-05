#pragma once

#include <string>
#include <vector>
#include <map>
#include <cstdint>
#include <variant>

#include <boost/variant.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/contains.hpp>
#include <boost/utility/enable_if.hpp>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace util::parser {

  class CFNull;
  class CFBoolean;
  class CFInteger;
  class CFReal;
  class CFString;
  class CFUnicodeString;
  class CFDate;
  class CFData;
  class CFDictionary;
  class CFArray;

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  typedef boost::variant <
    CFNull, 
    CFBoolean, 
    CFInteger, 
    CFReal, 
    CFString, 
    CFUnicodeString, 
    CFDate, 
    CFData, 
    boost::recursive_wrapper<CFArray>,
    boost::recursive_wrapper<CFDictionary>
  > CFType;

  int const CF_NULL_      = 0x00;
  int const CF_TRUE       = 0x09;
  int const CF_FALSE      = 0x08;
  int const CF_FILLER     = 0x0F;
  int const CF_INTEGER    = 0x1 << 4;
  int const CF_REAL       = 0x2 << 4;
  int const CF_DATE       = 0x3 << 4;
  int const CF_DATA       = 0x4 << 4;
  int const CF_STRING     = 0x5 << 4;
  int const CF_UNICODE    = 0x6 << 4;
  int const CF_STRING_NA  = 0x7 << 4;
  int const CF_UID        = 0x8 << 4;
  int const CF_UID_NA     = 0x9 << 4;
  int const CF_ARRAY      = 0xA << 4;
  int const CF_ARRAY_NA   = 0xB << 4;
  int const CF_SET        = 0xC << 4;
  int const CF_DICTIONARY = 0xD << 4;
  int const CF_DICT_NA    = 0xE << 4;

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFNull
  {
  public:
    CFNull() = default;

   ~CFNull() = default;

  private:
    /* data */
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFBoolean
  {
  public: // ABC
    CFBoolean();
    CFBoolean(bool v);

  public:
    auto to_xml(uint8_t indent=0) const -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;
    auto count_objects() -> uint32_t;
    auto value() const -> bool;

  private:
    bool m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // TODO: need template for various integer type
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFInteger
  {
  public:
    CFInteger();
    CFInteger(int64_t v);

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;
    auto count_objects() -> uint32_t;
    auto value() const -> int64_t;

    static int64_t as_integer(CFType t);

  private:
    int64_t m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // 
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFReal
  {
  public:
    CFReal();
    CFReal(double v);

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;
    auto to_s() -> std::string;

    auto count_objects() -> uint32_t;
    auto value() const -> double;

  private:
    double m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFUnicodeString
  {
  public:
    CFUnicodeString();
    CFUnicodeString(std::wstring  str);

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;

    auto count_objects() -> uint32_t;
    auto size() -> uint32_t;
    auto value() -> std::wstring;

  private:
    std::wstring m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFString
  {
  public:
    CFString();
    CFString(char const* str);
    CFString(std::string  str);

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;

    auto count_objects() -> uint32_t;
    auto size() -> uint32_t;
    auto value() -> std::string;

    static std::string 
      as_string(CFType t);

  private:
    std::string m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFDate
  {
  public:
    CFDate();
    CFDate(double v);

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;

    auto count_objects() -> uint32_t;
    auto size() -> uint32_t;
    auto value() -> std::string;

  private:
    auto to_s() const -> std::string;

  private:
    double m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFData
  {
  public:
    CFData();
    CFData(std::string  str);

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;

    auto count_objects() -> uint32_t;
    auto size() -> uint32_t;
    auto value() -> std::string;

  private:
    std::string m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFArray
  {
  public:
    typedef std::vector<CFType>::iterator iterator;

  public:
    CFArray();

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;

    auto count_objects() -> uint32_t;

  public:
    auto size() const -> size_t;
    auto reset() -> CFArray&;

    auto add(CFType const& value) -> CFArray&;
    auto add(int value) -> CFArray&;
    auto add(int64_t value) -> CFArray&;
    auto add(char const* value) -> CFArray&;
    auto add(std::string const& value) -> CFArray&;

    auto add_date(double value) -> CFArray&;

    static auto as_strings(CFType t)-> std::vector<std::string>;
    static auto as_integers(CFType t) -> std::vector<int64_t>;

  public:
    auto operator[](uint32_t index) -> CFType&;
    auto begin()   { return m_value.begin(); }
    auto end()     { return m_value.end();   }

  private:
    auto push_back(CFType const& value) -> void;

  public:
    std::vector<CFType> m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  class CFDictionary
  {
  public:
    typedef std::pair<std::string, CFType> value_type;
    typedef std::vector<value_type>::iterator iterator;

  public:
    CFDictionary();

  public:
    auto to_xml(uint8_t indent=0) -> std::string;
    auto to_bin(std::vector<uint8_t>& blist) -> void;

    auto add(CFType key, CFType b) -> CFDictionary&;
    auto add(char const* key, CFType b) -> CFDictionary&;
    auto add(char const* key, char const* value) -> CFDictionary&;
    auto add(std::string const& key, std::string const& value) -> CFDictionary&;

    auto count_objects() -> uint32_t;

    static CFDictionary 
      as_dictionary(CFType t);

    static std::map<std::string, std::string> 
      as_strings(CFType t);

    static std::map<std::string, int64_t> 
      as_integers(CFType t);

  public:
    auto begin() { return m_value.begin(); }
    auto end()   { return m_value.end();   }
    auto find(std::string const& key) -> CFDictionary::iterator;

  private:
    std::vector<value_type> m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // Generic Visitor
  //
  ////////////////////////////////////////////////////////////////////////////////
  template <typename Visitor, typename TL> 
  struct generic_visitor: boost::static_visitor<>, Visitor
  {
    template <typename T>
    void operator() (T v, 
          typename boost::enable_if<typename boost::mpl::contains<TL, T>::type>::type* =NULL) const
    {
      Visitor::operator()(v);
    }

    template <typename T>
    void operator() (T /* v */, 
          typename boost::disable_if<typename boost::mpl::contains<TL, T>::type>::type* =NULL) const
    {}
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // vector enumerator
  //
  ////////////////////////////////////////////////////////////////////////////////
  template <typename T> 
  struct vector_enumerator;

  template <> 
  struct vector_enumerator<std::string>
  {
    typedef boost::mpl::vector<CFString, CFArray> filter;
    typedef generic_visitor<vector_enumerator, filter> visitor_type;

    void operator() (CFArray& arr) const 
    { 
      visitor_type visitor;
      for (size_t i=0; i<arr.size(); i++) 
        boost::apply_visitor(visitor, arr[uint32_t(i)]);

      m_value = visitor.result();
    }

    void operator() (CFString& v) const 
    { 
      m_value.push_back(v.value());
    }

    std::vector<std::string> result() { return m_value; }

  private:
    mutable std::vector<std::string> m_value;
  };

  template <> 
  struct vector_enumerator<int64_t>
  {
    typedef boost::mpl::vector<CFInteger, CFArray> filter;
    typedef generic_visitor<vector_enumerator, filter> visitor_type;

    void operator() (CFArray& arr) const 
    { 
      visitor_type visitor;
      for (size_t i=0; i<arr.size(); i++) 
        boost::apply_visitor(visitor, arr[uint32_t(i)]);

      m_value = visitor.result();
    }

    void operator() (CFInteger& v) const 
    { 
      m_value.push_back(v.value());
    }

    std::vector<int64_t> result() { return m_value; }

  private:
    mutable std::vector<int64_t> m_value;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  template <typename Key, typename Value> 
  struct map_enumerator;

  template <> 
  struct map_enumerator<std::string, std::string>
  {
    typedef boost::mpl::vector<CFString, CFDictionary> filter;
    typedef generic_visitor<map_enumerator, filter> visitor_type;

    void operator() (CFDictionary& dict) const 
    { 
      visitor_type visitor;

      for (auto it= dict.begin(); it != dict.end(); ++it)
      {
        boost::apply_visitor(visitor, it->second);
        m_map[it->first] = visitor.value();
      }
    }

    void operator() (CFString& s) const 
    { 
      m_value = s.value(); 
    }

    std::map<std::string, std::string> 
      result() { return m_map; }

    std::string value() { return m_value; }

  private:
    mutable std::map<std::string, std::string> m_map;
    mutable std::string m_value;
  };

  template <> 
  struct map_enumerator<std::string, int64_t>
  {
    typedef boost::mpl::vector<CFInteger, CFDictionary> filter;
    typedef generic_visitor<map_enumerator, filter> visitor_type;

    void operator() (CFDictionary& dict) const 
    { 
      visitor_type visitor;
      for (auto it=dict.begin(); it != dict.end(); ++it)
      {
        boost::apply_visitor(visitor, it->second);
        m_map[it->first] = visitor.value();
      }
    }

    void operator() (CFInteger& i) const 
    { 
      m_value = i.value(); 
    }

    std::map<std::string, int64_t> 
      result() { return m_map; }

    int64_t value() { return m_value; }

  private:
    mutable std::map<std::string, int64_t> m_map;
    mutable int64_t m_value;

  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  struct xml_builder
  {
    typedef boost::mpl::vector<
      CFBoolean, CFInteger, CFReal, CFString, CFDate, CFData, CFArray, CFDictionary
    > filter;

    typedef generic_visitor<xml_builder, filter> visitor_type;

    xml_builder() : m_result("")
    {
      m_indent = 0;
    }

    void operator() (CFBoolean& b)    const { m_result += b.to_xml(m_indent); }
    void operator() (CFInteger& i)    const { m_result += i.to_xml(m_indent); }
    void operator() (CFReal& r)       const { m_result += r.to_xml(m_indent); }
    void operator() (CFString& s)     const { m_result += s.to_xml(m_indent); }
    void operator() (CFDate& d)       const { m_result += d.to_xml(m_indent); }
    void operator() (CFData& a)       const { m_result += a.to_xml(m_indent); }
    void operator() (CFArray& a)      const { m_result += a.to_xml(m_indent); }
    void operator() (CFDictionary& d) const { m_result += d.to_xml(m_indent); }

    std::string result()
    {
      return m_result;
    }

    void set_indent(uint8_t indent)
    {
      m_indent = indent;
    }

  private:
    mutable std::string m_result;
    uint8_t m_indent;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  struct type_visitor: boost::static_visitor<int>
  {
    int operator() (CFNull&)       const { return CF_NULL_; } 
    int operator() (CFBoolean&)    const { return CF_TRUE | CF_FALSE; } 
    int operator() (CFInteger&)    const { return CF_INTEGER;    }
    int operator() (CFReal&)       const { return CF_REAL;       }
    int operator() (CFString&)     const { return CF_STRING;     }
    int operator() (CFDate&)       const { return CF_DATE;       }
    int operator() (CFData&)       const { return CF_DATA;       }
    int operator() (CFArray&)      const { return CF_ARRAY;      }
    int operator() (CFDictionary&) const { return CF_DICTIONARY; }
    int operator() (CFUnicodeString&) const { return CF_UNICODE; }
  };

} 

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
