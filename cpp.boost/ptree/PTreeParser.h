#ifndef PTREEPARSER_H_PPDMXE3I
#define PTREEPARSER_H_PPDMXE3I

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/format.hpp>

#include <exception>
#include <vector>
#include <utility>

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
    PTreeParser& clear()
    void         reduce_to(std::vector<std::string>& res);

    Leaves       pairs();
    std::vector<std::string> 
                 strings()

private:
    Leaves map(Leaves const& r, std::string const& filter);

    // TODO exchange with lambda
    bool is_bool(string const& s);
    Range find_range(string const& path, int position)

private:
    std::string m_path;
    std::string m_ignore;
    Leaves m_pairs;
    vector<std::string> m_strs;
    ptree m_pt;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
