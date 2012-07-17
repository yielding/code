#include "stdafx.h"
#include "PTreeParser.h"

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
// constructor should prepare stream
//
////////////////////////////////////////////////////////////////////////////////
PTreeParser::PTreeParser()
    : m_ignore("...")
{
}

bool PTreeParser::init_with_path(string const& path)
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
bool PTreeParser::init_with_bplist(char* contents, size_t size)
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

bool PTreeParser::init_with_stream(stringstream& contents)
{
    return init(contents);
}

bool PTreeParser::init(stringstream& ss)
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

string PTreeParser::get_string(char const* key, char const* where) const
{
    return find_value(key, where);
}

string PTreeParser::get_string(char const* key) const
{
    return m_where.empty() 
        ? "" 
        : get_string(key, m_where.c_str());
}    

int PTreeParser::get_int(char const* key, char const* where) const
{
    auto res = get_string(key, where);

    return res.empty() ? -1 : boost::lexical_cast<int>(res);
}

int PTreeParser::get_int(char const* key) const
{
    auto res = get_string(key);

    return res.empty() ? -1 : boost::lexical_cast<int>(res);
}
  
PTreeParser::Leaves PTreeParser::get_dict(char const* key, char const* where) const
{
  return enumerate(key, where);
}

PTreeParser::Leaves PTreeParser::get_dict(char const* key) const
{
    return m_where.empty()
        ? Leaves()
        : get_dict(m_where.c_str(), key);    
}

PTreeParser::Leaves PTreeParser::enumerate(string const& key, char const* where) const
{
    auto rng = find_range_of_dict(key, where);
    auto beg = rng.first;
    auto end = rng.second;

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

bool PTreeParser::is_bool(string const& s) const
{ 
    return s == "true" || s == "false"; 
}

PTreeParser::Leaves PTreeParser::unfold(Leaves const& r) const
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
////////////////////////////////////////////////////////////////////////////////
string PTreeParser::find_value(string const& key, char const* where_) const
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

PTreeParser::Range PTreeParser::find_range_of_dict(std::string const& value, char const* where_) const
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
