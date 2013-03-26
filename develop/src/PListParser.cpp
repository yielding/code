#include "stdafx.h"
#include "PListParser.h"

#if defined(USE_BPLIST)
#include "BPlist.h"
#endif

#pragma warning( push )
#pragma warning( disable : 4503 )
#pragma warning( disable : 4819 )
#include <boost/property_tree/xml_parser.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>
#include <utility>
#include <sstream>
#include <exception>
#pragma warning( pop )

using namespace std;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
    template <typename ForwardIterator> 
    string join(ForwardIterator first, ForwardIterator last, string const& sep=", ")
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
// constructor should prepare stream
//
////////////////////////////////////////////////////////////////////////////////
PListParser::PListParser()
    : m_ignore("...")
{
}

bool PListParser::init_with_path(string const& path)
{
    if (!fs::exists(path))
        return false;

#if defined(USE_BPLIST)
    utility::parser::PropertyList bplist;
    if (bplist.open(path))
    {
        m_stream << bplist.to_xml();
        return init();
    }
#endif

    ifstream ifs(path.c_str(), ios_base::binary);
    if (!ifs.is_open())
        return false;

    auto   size = size_t(fs::file_size(path));
    auto buffer = new char[size];
    ifs.read(buffer, size);
    stringstream ss;
    ss.write(buffer, size);
    delete [] buffer;

    return init(ss);
}

#if defined(USE_BPLIST)
bool PListParser::init_with_bplist(char* contents, size_t size)
{
    utility::parser::PropertyList bplist;
    if (bplist.load(contents, size))
    {
        m_stream << bplist.to_xml();
        return init();
    }

    return false;
}
#endif

bool PListParser::init_with_string(string const& contents)
{
    stringstream ss;
    ss << contents;
  
    return init_with_stream(ss);
}
  
bool PListParser::init_with_stream(stringstream& contents)
{
    return init(contents);
}

bool PListParser::init(stringstream& ss)
{
    try 
    {
        read_xml(ss, m_pt); 
        return true;
    }
    catch (xml_parser_error&) 
    { 
        return false; 
    }

    return false;
}

string PListParser::get_string(char const* key, char const* where) const
{
    return find_value(key, where);
}

string PListParser::get_string(char const* key) const
{
    return m_where.empty() 
        ? "" 
        : get_string(key, m_where.c_str());
}    

int PListParser::get_int(char const* key, char const* where) const
{
    auto res = get_string(key, where);

    return res.empty() ? -1 : boost::lexical_cast<int>(res);
}

int PListParser::get_int(char const* key) const
{
    auto res = get_string(key);

    return res.empty() ? -1 : boost::lexical_cast<int>(res);
}
  
auto PListParser::get_dict(char const* key, char const* where) const
  -> map<string, string>
{
  return enumerate(key, where);
}

auto PListParser::get_dict(char const* key) const
  -> map<string, string>
{
    map<string, string> empty_;
    return m_where.empty()
        ? empty_
        : get_dict(m_where.c_str(), key);    
}

auto PListParser::enumerate(string const& key, char const* where) const
  -> map<string, string>
{
    auto tree = find_dict(key, where);
    auto  beg = tree.begin();
    auto  end = tree.end();

    Leaves result;
    for ( ; beg != end; ++beg)
    {
        auto key = beg->first.data();
        auto val = beg->second.empty() ? beg->second.data() : m_ignore;
        if (val.empty()) 
            val = m_ignore;

        result.push_back(make_pair(key, val));
    }

    return unfold(result);
}

bool PListParser::is_bool(string const& s) const
{ 
    return s == "true" || s == "false"; 
}

auto PListParser::unfold(Leaves const& r) const -> map<string, string>
{
    map<string, string> result;

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
            result[keep] = to_add;
        }
    }

    return result;
}

////////////////////////////////////////////////////////////////////////////////
//
// what's the range?
//
////////////////////////////////////////////////////////////////////////////////
string PListParser::find_value(string const& key, char const* where_) const
{
    string empty_value("");

    vector<string> paths;
    auto node = find(m_pt, key, paths);
    if (!node.first)
        return empty_value;

    reverse(paths.begin(), paths.end());
    auto path = join(paths.begin(), paths.end(), ".");
    if (path != where_)
        return empty_value;
    
    auto result = node.second->second.data();
    return result.empty()
        ? node.second->first.data()         // <true/> <false/>
        : result;
}

auto PListParser::find_dict(string const& value, char const* where_) const -> ptree
{
    ptree empty_tree;

    vector<string> paths;
    auto node = find(m_pt, value, paths);
    if (!node.first)
        return empty_tree;

    reverse(paths.begin(), paths.end());
    auto path = join(paths.begin(), paths.end(), ".");
    return (path != where_)
        ? empty_tree
        : node.second->second;
}

auto PListParser::find(ptree const& pt, string const& key, vector<string>& paths) const
    -> pair<bool, ptree::const_iterator>
{
    auto end = pt.end();
    for (ptree::const_iterator it = pt.begin(); it != end; ++it)
    {
        auto first  = it->first;
        auto second = it->second.get_value<string>();
            
        if (first == "<xmlattr>")
            continue;
            
        if (first == "key" && second == key)
            return make_pair(true, ++it);     // omit having the path
            
        auto result = find(it->second, key, paths);
        if (result.first)
        {
            paths.push_back(first);
            return result;
        }
    }

    return make_pair(false, pt.begin());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}
}
