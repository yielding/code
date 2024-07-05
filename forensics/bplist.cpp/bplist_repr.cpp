#include "bplist_repr.hpp"
#include "base64.hpp"

#include <boost/algorithm/string.hpp>
#include <utility>
#include <format>

#if defined(WIN32) && defined(_DEBUG)
#define new DEBUG_NEW
#endif

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  auto tabs(size_t indent) -> string
  {
    string res; 
    for (size_t i=0; i<indent; i++) res += "\t";

    return res;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace util::parser {

  CFBoolean::CFBoolean(): m_value(false) 
  {
  }

  CFBoolean::CFBoolean(bool v): m_value(v) 
  {
  }

  auto CFBoolean::to_xml(uint8_t indent) const -> string
  { 
    auto value = m_value ? "<true/>"s : "<false/>"s;

    return tabs(indent) + value + "\n";
  }

  auto CFBoolean::to_bin(vector<uint8_t>& /*bplist*/) -> void
  {
  }

  auto CFBoolean::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFBoolean::value() const -> bool
  {
    return m_value;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // NOTICE: need template for various integer type
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFInteger::CFInteger(): m_value(0)
  {
  }

  CFInteger::CFInteger(int64_t v): m_value(v)
  {
  }

  auto CFInteger::to_xml(uint8_t indent) -> string
  { 
    using namespace boost;

    return tabs(indent) + format("<integer>{}</integer>\n", m_value);
  }

  void CFInteger::to_bin(vector<uint8_t>& /*blist*/)
  {
  }

  auto CFInteger::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFInteger::value() const -> int64_t
  {
    return m_value;
  }

  auto CFInteger::as_integer(CFType t) -> int64_t
  {
    CFInteger* res = boost::get<CFInteger>(&t);

    return res ? res->value() : 0;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // 
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFReal::CFReal(): m_value(0.0)
  {
  }

  CFReal::CFReal(double v): m_value(v)
  {
  }

  auto CFReal::to_xml(uint8_t indent) -> string
  { 
    return format("{}<real>{}</real>\n", tabs(indent), m_value);
  }

  auto CFReal::to_s() -> string
  {
    return format("{}", m_value);
  }

  auto CFReal::to_bin(vector<uint8_t>& /*blist*/) -> void
  {
  }

  auto CFReal::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFReal::value() const -> double
  {
    return m_value;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFUnicodeString::CFUnicodeString()
  {
  }

  CFUnicodeString::CFUnicodeString(wstring  str) : m_value(std::move(str))
  {
  }

  auto CFUnicodeString::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFUnicodeString::to_xml(uint8_t /*indent*/) -> string
  {
    return "";
  }

  auto CFUnicodeString::to_bin(vector<uint8_t>& /*blist*/) -> void
  {
  }

  auto CFUnicodeString::size() -> uint32_t
  {
    return uint32_t(m_value.size());
  }

  auto CFUnicodeString::value() -> wstring
  {
    return m_value;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFString::CFString()
  {
  }

  CFString::CFString(char const* str) : m_value(str)
  {
  }

  CFString::CFString(string str) : m_value(std::move(str))
  {
  }

  auto CFString::to_xml(uint8_t indent) -> string
  {
    string str = boost::replace_all_copy(m_value, "\"", "&quot");
    boost::replace_all(str, "<", "&lt");
    boost::replace_all(str, ">", "&gt");
    boost::replace_all(str, "&", "&amp");

    return tabs(indent) + string("<string>") + str + string("</string>\n");
  }

  auto CFString::to_bin(vector<uint8_t>& /*blist*/) -> void
  {
  }

  auto CFString::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFString::size() -> uint32_t
  {
    return uint32_t(m_value.size());
  }

  auto CFString::value() -> string
  {
    return m_value;
  }

  auto CFString::as_string(CFType t) -> string
  {
    auto s = boost::get<CFString>(&t);

    return s ? s->value() : string();
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFDate::CFDate()
  {
    m_value = 0;
  }

  CFDate::CFDate(double v)
  {
    m_value = v;
  }

  auto CFDate::to_xml(uint8_t indent) -> string
  {
    return tabs(indent) + format("<date>{}</date>\n", to_s());
  }

  auto CFDate::to_s() const -> string
  {
    char buf[32] = { 0 };

    // REMARK
    // t가 아이폰의 epoch에 기준을 두고 있다는 가정의 코드

    // mac의 epoch는 2001/1/1을 기준으로 한다.
    // unix timestamp는 1970/1/1을 기준으로 하므로 1970/1/1 ~2001/1/1 만큼의 초를 더한다.
    // (31*365 + 8:leap) * 24 * 60 * 60 = 978307200
    // 반면 FILETIME은 1601/1/1을 기준으로 100-nanosecond단위의 시간

    auto t = (time_t)m_value; t += 978307200;
    if (t <= 0)
      return "";

    std::tm* g = gmtime(&t);
    if (g == nullptr)
      return "";

    strftime(buf, 32, "%Y-%m-%dT%H:%M:%SZ", g);

    return string(buf);
  }

  auto CFDate::to_bin(vector<uint8_t>& /*blist*/) -> void
  {
  }

  auto CFDate::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFDate::size() -> uint32_t
  {
    return 4;
  }

  // REFACTOR
  auto CFDate::value() -> string
  {
    return to_s();
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFData::CFData()
  {
  }

  CFData::CFData(string  str) : m_value(std::move(str))
  {
  }

  auto CFData::to_xml(uint8_t indent) -> string
  {
    using namespace util::codec;

    auto space = tabs(indent);

    return space + string("<data>\n") + 
      base64::encode(m_value) + string("\n") + 
      space + string("</data>\n");
  }

  auto CFData::to_bin(vector<uint8_t>& /*bplist*/) -> void
  {
  }

  auto CFData::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFData::size() -> uint32_t
  {
    return uint32_t(m_value.size());
  }

  auto CFData::value() -> string
  {
    return m_value;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFArray::CFArray() = default;

  auto CFArray::to_xml(uint8_t indent) -> string
  {
    auto space = tabs(indent);
    auto res   = space + "<array>\n";

    for (auto& i : m_value)
    {
      xml_builder::visitor_type visitor;
      visitor.set_indent(indent+1);
      apply_visitor(visitor, i);
      res += visitor.result();
    }

    res += space + "</array>\n";

    return res;
  }

  auto CFArray::to_bin(vector<uint8_t>& /*bplist*/) -> void
  {
  }

  auto CFArray::count_objects() -> uint32_t
  { 
    return 1; 
  }

  auto CFArray::add(CFType const& value) -> CFArray&
  {
    m_value.push_back(value);

    return *this;
  }

  auto CFArray::add(int value) -> CFArray&
  {
    m_value.emplace_back(CFInteger(value));

    return *this;
  }

  auto CFArray::add(int64_t value) -> CFArray&
  {
    m_value.emplace_back(CFInteger(value));

    return *this;
  }

  auto CFArray::add(char const* value) -> CFArray&
  {
    m_value.emplace_back(CFString(string(value)));

    return *this;
  }

  auto CFArray::add(string const& value) -> CFArray&
  {
    m_value.emplace_back(CFString(value));

    return *this;
  }

  auto CFArray::add_date(double value) -> CFArray&
  {
    m_value.emplace_back(CFDate(value));

    return *this;
  }

  auto CFArray::push_back(CFType const& value) -> void 
  {
    m_value.push_back(value);
  }

  auto CFArray::size() const -> size_t
  {
    return m_value.size();
  }

  auto CFArray::reset() -> CFArray&
  {
    m_value.clear();
    return *this;
  }

  auto CFArray::operator[] (uint32_t index) -> CFType&
  {
    return m_value[index];
  }

  auto CFArray::as_strings(CFType t) -> vector<string>
  {
    vector_enumerator<string>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
  }

  auto CFArray::as_integers(CFType t) -> vector<int64_t>
  {
    vector_enumerator<int64_t>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  ////////////////////////////////////////////////////////////////////////////////
  CFDictionary::CFDictionary()
  {}

  auto CFDictionary::to_xml(uint8_t indent) -> string
  {
    auto space = tabs(indent);
    auto res = space + "<dict>\n";

    for (auto& i : m_value)
    {
      res += tabs(indent+1) + string("<key>") + i.first + "</key>\n";
      xml_builder::visitor_type visitor;
      visitor.set_indent(indent+1);
      apply_visitor(visitor, i.second);
      res += visitor.result();
    }

    res += space + "</dict>\n";

    return res;
  }

  auto CFDictionary::to_bin(vector<uint8_t>& /*blist*/) -> void
  {
  }

  auto CFDictionary::add(CFType key, CFType value) -> CFDictionary& 
  {
    auto key_str = boost::get<CFString>(&key);
    if (key_str != nullptr)
      m_value.emplace_back(key_str->value(), value);

    return *this;
  }

  auto CFDictionary::add(char const* key, CFType b) -> CFDictionary& 
  {
    m_value.emplace_back(string(key), b);

    return *this;
  }

  auto CFDictionary::add(char const* key, char const* value) -> CFDictionary& 
  {
    this->add(string(key), string(value));

    return *this;
  }

  auto CFDictionary::add(string const& key, string const& value) -> CFDictionary& 
  {
    m_value.emplace_back(key, CFString(value));

    return *this;
  }

  auto CFDictionary::count_objects() -> uint32_t 
  { 
    return 1; 
  }

  auto CFDictionary::as_dictionary(CFType t) -> CFDictionary 
  {
    auto res = boost::get<CFDictionary>(&t);

    return res ? *res : CFDictionary();
  }

  auto CFDictionary::as_strings(CFType t) -> map<string, string> 
  {
    map_enumerator<string, string>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
  }

  auto CFDictionary::as_integers(CFType t) -> map<string, int64_t> 
  {
    map_enumerator<string, int64_t>::visitor_type v;
    boost::apply_visitor(v, t);

    return v.result();
  }

  auto CFDictionary::find(std::string const& key) -> CFDictionary::iterator
  {
    auto it = m_value.begin();
    while (it != m_value.end())
    {
      if (it->first == key) break;
      ++it;
    }

    return it;
  }

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

#if 0
#include <iostream>

using namespace std;

struct value_adder
{
  typedef boost::mpl::vector<CFInteger, CFString, CFArray> filter;
  typedef generic_visitor<value_adder, filter> visitor_type;

  value_adder() : m_total(0)
  {}

  void operator() (CFString& s) const
  {
    // cout << s.value();
  }

  void operator() (CFInteger& i) const
  {
    // cout << i.value() << " ";
    m_total += i.value();
  }

  void operator() (CFArray& v) const
  {
    cout << "( ";
    visitor_type visitor;

    for (size_t i=0; i<v.size(); i++) apply_visitor(visitor, v.m_value[i]);
    m_total += visitor.result();

    cout << ") ";
  }

  int result()
  {
    return m_total;
  }

private:
  mutable int m_total;
};

CFType return_int()
{
  CFInteger i(1);

  return i;
}

int main(int argc, char * const argv[])
{
  CFType root;

  CFArray b;
  for (int i=0; i<10; i++) 
    b.add(CFInteger(i));

  b.add(b);
  CFArray c;
  c.add(CFInteger(100));
  c.add(CFInteger(200));
  c.add(CFString("leech"));
  c.add(c);
  c.add(CFInteger(50));
  c.add(c);
  root = c;

  value_adder::visitor_type v;
  boost::apply_visitor(v, root);

  cout << endl << v.result() << endl;

  return_int();

  return 0;
}

#endif

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
