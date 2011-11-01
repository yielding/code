#ifndef PTREEPARSER_H_PPDMXE3I
#define PTREEPARSER_H_PPDMXE3I

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

#include <vector>
#include <string>
#include <map>

using namespace boost::property_tree;

namespace utility { namespace parser {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class PTreeParser
{
public:
  typedef std::vector<std::pair<std::string, std::string> > Leaves; 
  typedef std::pair<ptree::iterator, ptree::iterator> Range;

public:
  PTreeParser(std::string const& path, std::string const& ignore="...");
  
  bool init();
  bool init_with(char const* where);

public:
  auto get_dict_with(char const* where, char const* key) -> Leaves;
  auto get_dict_with(char const* key) -> Leaves;
  
  auto get_string_with(char const* where, char const* key) -> std::string;
  auto get_string_with(char const* key) -> std::string;

  auto get_int_with(char const* where, char const* key) -> int;
  auto get_int_with(char const* key) -> int;
  
public:
  auto enumerate(std::string const& key, char const* where)
    -> Leaves;
  
  auto unfold(Leaves const& r)
    -> Leaves;

  auto is_bool(std::string const& s) -> bool;

  // to delete
  // auto filter(std::string const& key) -> PTreeParser&;

  // auto clear() -> PTreeParser&;
  // auto reduce_to(std::vector<std::string>& res) -> void;
  // auto pairs() -> Leaves;
  // auto strings() -> std::vector<std::string>;

private:
  auto find_value(std::string const& key, char const* where_)
    -> std::string;

  auto find_range_of_dict(std::string const& key, char const* where) 
    -> Range;

private:
  std::string m_path;
  std::string m_ignore;
  std::string m_where;
  // to delete
  // Leaves m_pairs;
  // std::vector<std::string> m_strs;
  ptree m_pt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
#endif
