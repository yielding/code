#pragma once

#include <boost/property_tree/ptree.hpp>

#include <map>
#include <vector>
#include <string>
#include <utility>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace util::parser {

  using namespace boost::property_tree;

  using std::vector;
  using std::map;
  using std::string;
  using std::pair;
  using std::stringstream;

  class PListParser
  {
  private:
    typedef vector<pair<string, string>> Leaves;

  public:
    PListParser();

    bool init_with_stream(stringstream& conents);
    bool init_with_string(string const& contents);
    bool init_with_path(string const& path);
    #if defined(USE_BPLIST)
    bool init_with_bplist(char* contents, size_t size);
    #endif

    PListParser& in(char const* w) { m_where = w; return *this; }

  public:
    auto get_dict(char const* key, char const* where) const -> map<string, string>;
    auto get_dict(char const* key) const -> map<string, string>;

    auto get_string(char const* key, char const* where) const -> string;
    auto get_string(char const* key) const -> string;

    auto get_int(char const* key, char const* where) const -> int;
    auto get_int(char const* key) const -> int;

  public:
    auto enumerate(string const& key, char const* where) const -> map<string, string>;
    auto unfold(Leaves const& r) const -> map<string, string>;

    bool is_bool(string const& s) const;

  private:
    bool init(stringstream& s);

  private:
    auto find_value(string const& key, char const* where_) const -> string;

    auto find_dict(string const& key, char const* where) const -> ptree;

    auto find(ptree const& pt, string const& key, vector<string>&) const
      -> pair<bool, ptree::const_iterator>;

  private:
    string m_ignore;
    string m_where;

    ptree m_pt;
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
