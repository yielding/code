#include "stdafx.h"
#include "PTreeParser.h"

#include <boost/format.hpp>

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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
PTreeParser::PTreeParser(string const& path, string const& ignore)
  : m_path(path), m_ignore(ignore)
{}

bool PTreeParser::init()
{
    try 
    {
        if (m_bplist.open(m_path))
        {
            stringstream ss; ss << m_bplist.to_xml();
            read_xml(ss, m_pt); 
        }
        else
        {
            read_xml(m_path, m_pt); 
        }

    }
    catch (xml_parser_error&) 
    { 
        return false; 
    }

    return true;
}

PTreeParser& PTreeParser::enumerate(string const& path, int index, string const& filt)
{
    Leaves result;
    auto   r = find_range(path, index);
    auto beg = r.first, end = r.second;

    while (beg != end)
    {
        string const& key = beg->first.data();
        string val = beg->second.empty() ? beg->second.data() : m_ignore;
        if (val.empty()) 
            val = m_ignore;

        search(filt.begin(), filt.end(), key.begin(), key.end()) != filt.end()
            ? result.push_back(make_pair(key, val))
            : result.pop_back();

        ++beg;
    }

    auto mapped = map(result, filt);
    copy(mapped.begin(), mapped.end(), back_inserter(m_pairs));

    return *this;
}

void PTreeParser::reduce_to(vector<string>& res)
{
    copy(m_strs.begin(), m_strs.end(), back_inserter(res));
}

PTreeParser& PTreeParser::filter(string const& key)
{
    m_strs.clear();

    for (auto it = m_pairs.begin(); it != m_pairs.end(); ++it)
    {
        auto const& to_push = (key == "first") ? it->first : it->second;
        m_strs.push_back(to_push);
    }

    return *this;
}

PTreeParser::Leaves PTreeParser::pairs()
{
    return m_pairs;
}

vector<string> PTreeParser::strings()
{
    return m_strs;
}

PTreeParser& PTreeParser::clear()
{
    m_strs.clear(); m_pairs.clear();

    return *this;
}

PTreeParser::Leaves PTreeParser::map(Leaves const& r, string const& filter)
{
    Leaves result;
    // auto is_bool = [](string const& s) { return s == "true" || s == "false"; }

    string keep;
    for (auto it = r.begin(); it != r.end(); ++it)
        if (it->first == "key")
        {
            keep = it->second;
        }
        else
        {
            auto to_add = is_bool(it->first) ? it->first : it->second;
            result.push_back(make_pair(keep, to_add));
        }

    return result;
}

bool PTreeParser::is_bool(string const& s) 
{ 
    return s == "true" || s == "false"; 
}

PTreeParser::Range PTreeParser::find_range(string const& path, int position)
{
    auto found  = path.find_last_of(".");
    auto parent = path.substr(0, found);
    auto key    = path.substr(found+1);
    auto beg    = m_pt.get_child(parent).begin();
    auto end    = m_pt.get_child(parent).end();

    for (int index=0; beg != end; ++beg)
    {
        if (beg->first.data() != key)
            continue;

        if (position == index)
            return make_pair(beg->second.begin(), beg->second.end());

        ++index;
    }

    return make_pair(m_pt.end(), m_pt.end());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
