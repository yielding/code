#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/format.hpp>

#include <exception>
#include <vector>
#include <utility>
#include <iostream>
#include <sstream>

using namespace std;
// using boost::property_tree::ptree;
using namespace boost::property_tree;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class PTreeParser
{
public:
    typedef vector<pair<string, string>> Leaves; 
    typedef pair<ptree::iterator, ptree::iterator> Range;

public:
    PTreeParser(string const& path, string const& ignore="...")
        : m_path(path), m_ignore(ignore)
    {}

    bool init()
    {
        try 
        {
            read_xml(m_path, m_pt); 
        }
        catch(xml_parser_error&) 
        { return false; }

        return true;
    }

    PTreeParser& enumerate(string const& path, int index, string const& filt)
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

    void reduce_to(vector<string>& res)
    {
        copy(m_strs.begin(), m_strs.end(), back_inserter(res));
    }

    PTreeParser& filter(string const& key)
    {
        m_strs.clear();

        for (auto it = m_pairs.begin(); it != m_pairs.end(); ++it)
        {
            auto const& to_push = (key == "first") ? it->first : it->second;
            m_strs.push_back(to_push);
        }

        return *this;
    }

    Leaves pairs()
    {
        return m_pairs;
    }

    vector<string> strings()
    {
        return m_strs;
    }

    PTreeParser& clear()
    {
        m_strs.clear(); m_pairs.clear();

        return *this;
    }

private:
    Leaves map(Leaves const& r, string const& filter)
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

    // TODO exchange with lambda
    bool is_bool(string const& s) 
    { 
        return s == "true" || s == "false"; 
    }

    Range find_range(string const& path, int position)
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

private:
    string m_path;
    string m_ignore;
    Leaves m_pairs;
    vector<string> m_strs;
    ptree m_pt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename ForwardIterator> 
string join_(ForwardIterator first, ForwardIterator last, string const& sep=", ")
{
    stringstream ss;
    ss << *first++;
    for (; first != last; ++first) ss << sep << *first;

    return ss.str();
}

struct Manifest
{
    bool   was_passcode_set;
    bool   is_encrypted;
    string product_version;
    string product_type;
    vector<string> apps;

    string to_s()
    {
        auto passcode  = was_passcode_set ? "pass on" : "pass off";
        auto encrypted = is_encrypted ? "encrypted" : "not encrypted";
        auto settings  = boost::str(boost::format("%s\n%s\n%s\n%s\n") % passcode % encrypted % 
                product_version % product_type);
        auto apps_     = join_(apps.begin(), apps.end());
        return settings + "[" + apps_+ "]";
    } 
};

template <typename Leaves> 
void pr(Leaves const& l)
{
    for (int i=0; i<l.size(); i++) cout << l[i].first << ",[" << l[i].second << "]" << endl;

    cout << "------------------\n";
}

int main()
{
    try
    {
        PTreeParser ptree("/Users/yielding/develop/data/Manifest.xml");
        if (!ptree.init())
            return 0;

        string key1 = "plist.dict";
        string key2 = "plist.dict.dict";
        string filter = "key, string, true, false, date";
        auto l1 = ptree.enumerate(key1, 0, filter)
                       .enumerate(key2, 0, filter).pairs();

        Manifest m;
        for (auto it = l1.begin(); it !=l1.end(); ++it)
        {
            auto const& key = it->first; 
            if (key == "IsEncrypted")
                m.is_encrypted = it->second == "true";
            else if (key == "WasPasscodeSet")
                m.was_passcode_set = it->second == "true";
            else if (key == "ProductVersion")
                m.product_version = it->second;
            else if (key == "ProductType")
                m.product_type = it->second;
            else 
                continue;
        }

        ptree.clear()
             .enumerate(key2, 1, "key, string, dict")
             .filter("first")
             .reduce_to(m.apps);

        cout << m.to_s();
    }
    catch (std::exception &e)
    {
        std::cout << "error: " << e.what() << "\n";
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
