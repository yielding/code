#ifndef PTREEPARSER2_H_H6AC3ANN
#define PTREEPARSER2_H_H6AC3ANN

#include <boost/property_tree/ptree.hpp>

#include <map>
#include <vector>
#include <string>

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
    typedef std::vector<std::pair<std::string, std::string>> Leaves; 
    typedef std::pair<ptree::const_iterator, ptree::const_iterator> Range;

public:
    PTreeParser();

    bool init_with_stream(std::stringstream& conents);
    bool init_with_path(std::string const& path);
    #if defined(USE_BPLIST)
    bool init_with_bplist(char* contents, size_t size);
    #endif

    PTreeParser& in(char const* w) { m_where = w; return *this; }

public:
    Leaves get_dict(char const* where, char const* key) const;
    Leaves get_dict(char const* key) const;

    std::string get_string(char const* where, char const* key) const;
    std::string get_string(char const* key) const;

    int get_int(char const* where, char const* key) const;
    int get_int(char const* key) const;

public:
    Leaves enumerate(std::string const& key, char const* where) const;

    Leaves unfold(Leaves const& r) const;

    bool   is_bool(std::string const& s) const;

private:
    bool init(std::stringstream& s);

private:
    std::string find_value(std::string const& key, char const* where_) const;

    Range find_range_of_dict(std::string const& key, char const* where) const;

private:
    std::string m_ignore;
    std::string m_where;

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
