#ifndef PTREEPARSER_H_PPDMXE3I
#define PTREEPARSER_H_PPDMXE3I

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
////////////////////////////////////////////////////////////////////////////////
class PTreeParser
{
public:
    typedef std::vector<std::pair<std::string, std::string>> Leaves; 
    typedef std::pair<ptree::iterator, ptree::iterator> Range;

public:
    PTreeParser(std::string const& path, std::string const& ignore="...");
    bool init();

public:
    PTreeParser& enumerate(std::string const& path, int index, std::string const& filt);
    PTreeParser& filter(std::string const& key);
    PTreeParser& clear();
    void         reduce_to(std::vector<std::string>& res);

    Leaves       pairs();
    std::vector<std::string> 
                 strings();

private:
    Leaves map(Leaves const& r, std::string const& filter);

    // TODO exchange with lambda
    bool is_bool(std::string const& s);
    Range find_range(std::string const& path, int position);

private:
    std::string m_path;
    std::string m_ignore;
    Leaves m_pairs;
    std::vector<std::string> m_strs;
    ptree m_pt;
    utility::parser::PropertyList m_bplist;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
