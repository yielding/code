// #include "stdafx.h"
#include "PTreeParser.h"

#include <boost/lexical_cast.hpp>
#include <utility>
#include <sstream>
#include <exception>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  template <typename ForwardIterator> 
  string join_(ForwardIterator first, ForwardIterator last, string const& sep=", ")
  {
    stringstream ss;
    ss << *first++;
    for (; first != last; ++first) ss << sep << *first;

    return ss.str();
  }
}

namespace utility { namespace parser {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
PTreeParser::PTreeParser(string const& path, string const& ignore)
  : m_path(path)
  , m_ignore(ignore)
{}

bool PTreeParser::init()
{
  try 
  {
    read_xml(m_path, m_pt);
    return true;
  }
  catch (xml_parser_error&) 
  { 
    return false; 
  }

  return false;
}
  
bool PTreeParser::init_with(char const* where)
{
  if (!init()) 
    return false;
  
  m_where = where;
  
  return true;
}

auto PTreeParser::get_string_with(char const* key, char const* where) -> std::string
{
  return find_value(key, where);
}

auto PTreeParser::get_string_with(char const* key) -> std::string
{
  return m_where.empty() 
    ? "" 
    : get_string_with(key, m_where.c_str());
}    

auto PTreeParser::get_int_with(char const* where, char const* key) -> int
{
  auto res = get_string_with(key, where);

  return res.empty() ? -1 : boost::lexical_cast<int>(res);
}

auto PTreeParser::get_int_with(char const* key) -> int
{
  auto res = get_string_with(key);

  return res.empty() ? -1 : boost::lexical_cast<int>(res);
}
  
auto PTreeParser::get_dict_with(char const* where, char const* key)
  -> Leaves
{
  return enumerate(key, where);
}

auto PTreeParser::get_dict_with(char const* key)
  -> Leaves
{
  return m_where.empty()
    ? Leaves()
    : get_dict_with(m_where.c_str(), key);    
}

auto PTreeParser::enumerate(string const& key, char const* where)
  -> Leaves
{
  auto rng = find_range_of_dict(key, where);
  auto beg = rng.first, 
       end = rng.second;

  Leaves result;
  while (beg != end)
  {
    auto key = beg->first.data();
    auto val = beg->second.empty() ? beg->second.data() : m_ignore;
    if (val.empty()) 
      val = m_ignore;

    result.push_back(make_pair(key, val));

    ++beg;
  }

  return unfold(result);
}

bool PTreeParser::is_bool(string const& s) 
{ 
  return s == "true" || s == "false"; 
}

auto PTreeParser::unfold(Leaves const& r)
  -> PTreeParser::Leaves 
{
  Leaves result;

  string keep;
  for (auto it = r.begin(); it != r.end(); ++it)
  {
    if (it->first == "key")
    {
      keep = it->second;
    }
    else
    {
      auto to_add = is_bool(it->first) ? it->first : it->second;
      result.push_back(make_pair(keep, to_add));
    }
  }

  return result;
}

////////////////////////////////////////////////////////////////////////////////
//
// what's the range?
//
//  주어진 depth(where)와 key를 만족하는 iterator의 범위
//
////////////////////////////////////////////////////////////////////////////////
auto PTreeParser::find_value(string const& key, char const* where_)
  -> string
{
  string result;
  string where = (where_ != nullptr) ? where_ : m_where;
  if (where.find_last_of(".") == string::npos)
    return result;

  auto beg = m_pt.get_child(where).begin();
  auto end = m_pt.get_child(where).end();

  bool found = false;
  for (auto it=beg; it!=end; ++it)
  {
    if (found)
      return it->second.data();

    string f = it->first.data();
    string s = it->second.data();
    if (f == "key" && s == key)
      found = true;
  }

  return result;
}

// 
// 이걸 기준으로 나머지 전부 다 짠다.  무조건 되는 거지. 쫄지마 시바~
// 내가 진짜 오랬동안 헤맸던 이유는 node의 구조를 정확하게 이해하지 못한 것.
// second.data...
// first는 tag 이름이다. <key>, <xml> 등등
// second는 이 tag가 leaf일 경우 data에 스트링이 아닌 경우는 child들이 있는 것.
//
auto PTreeParser::find_range_of_dict(std::string const& value, char const* where_)
  -> Range
{
  Range r(m_pt.end(), m_pt.end());
  string where = (where_ != nullptr) ? where_ : m_where;
  if (where.find_last_of(".") == string::npos)
    return r;

  auto beg = m_pt.get_child(where).begin();
  auto end = m_pt.get_child(where).end();

  bool found = false;
  for (auto it=beg; it != end; ++it)
  {
    if (found)
    {
      r.first  = it->second.begin();
      r.second = it->second.end();
      break;
    }

    string k = it->first.data();
    string v = it->second.data();
    if (k == "key" && v == value)
      found = true;
  }

  return r;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
