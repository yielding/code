#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/algorithm/string.hpp>
#include <exception>
#include <vector>
#include <utility>
#include <iostream>

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

public:
    PTreeParser(string const& path, string const& ignore="...")
        :m_path(path), m_ignore(ignore)
    {}

    bool init()
    {
        try { read_xml(m_path, m_pt); } catch (xml_parser_error&) { return false; }
        return true;
    }

    Leaves enumerate(string const& path, string const& filt)
    {
        using namespace boost;

        vector<string> filts; split(filts, filt,  is_any_of(", "));

        auto pbeg = m_pt.get_child(path).begin();
        auto pend = m_pt.get_child(path).end();
        Leaves result;

        while (pbeg != pend)
        {
            string const& key = pbeg->first.data();
            string val = pbeg->second.empty() ? pbeg->second.data() : m_ignore;
            if (val.empty()) 
                val = m_ignore;

            find(filts.begin(), filts.end(), key) != filts.end()
                ? result.push_back(make_pair(key, val))
                : result.pop_back();

            ++pbeg;
        }

        return result;
    }

    virtual ~PTreeParser()
    {}

private:
    string m_path;
    string m_ignore;
    ptree m_pt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename Leaves> 
void pr(Leaves const& l)
{
    for (int i=0; i<l.size(); i++)
        cout << l[i].first << ",[" << l[i].second << "]" << endl;
    cout << "------------------\n";
}

int main()
{
    try
    {
        PTreeParser p("/Users/yielding/develop/data/Manifest.xml");
        if (!p.init())
            return 0;

        string key1 = "plist.dict";
        auto l1 = p.enumerate(key1, "key, string, true, false, date"); pr(l1);

        string key2 = "plist.dict.dict";
        auto l2 = p.enumerate(key2, "key, string, true, false, date"); pr(l2);

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
