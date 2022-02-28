#pragma once

#include "BPlist.h"

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

#include <vector>
#include <string>

using namespace boost::property_tree;

////////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////// namespace utility::parser {

  class PTreeParser
  {
  public:
    typedef std::vector<std::pair<std::string, std::string>> Leaves; 
    typedef std::pair<ptree::iterator, ptree::iterator> Range;

  public:
    PTreeParser(std::string const& path, std::string const& ignore="...");
    bool init();

  public:
    auto enumerate(std::string const& path, int index, std::string const& filt)
      -> PTreeParser&;
    auto filter(std::string const& key) -> PTreeParser&;
    auto clear() -> PTreeParser&;
    auto reduce_to(std::vector<std::string>& res) -> void;
    auto pairs() -> Leaves;
    auto strings() -> std::vector<std::string>;

  private:
    auto map(Leaves const& r, std::string const& filter) 
      -> Leaves;

    // TODO exchange with lambda
    auto is_bool(std::string const& s) -> bool;
    auto find_range(std::string const& path, int position) -> Range;

  private:
    std::string m_path;
    std::string m_ignore;
    Leaves m_pairs;
    std::vector<std::string> m_strs;
    ptree m_pt;
    utility::parser::PropertyList m_bplist;
  };

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
