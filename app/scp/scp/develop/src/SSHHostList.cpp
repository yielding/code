#include "SSHHostList.h"

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/qi.hpp>

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>

using namespace std;
using namespace boost;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace impl {
    struct HostInfo 
    { 
        string host, id, pw; 
    };
}

//
// BOOST_FUSION_ADAPT_STRUCT cannot be located inside the namespace
//
BOOST_FUSION_ADAPT_STRUCT(
    impl::HostInfo,
    (string, host) (string, id) (string, pw)
)

namespace impl { 

namespace    qi = spirit::qi;
namespace ascii = spirit::ascii;

template <typename Iterator>
struct HostInfoParser : qi::grammar<Iterator, HostInfo(), ascii::space_type>
{
    HostInfoParser() : HostInfoParser::base_type(start)
    {
        using namespace qi;
        qs    %= lexeme['"' >> +(char_ - '"') >> '"'];
        start %= lit("host") >> '{' >> qs >> ',' >> qs >> ',' >> qs >> '}';
    }

    qi::rule<Iterator, string(), ascii::space_type> qs;
    qi::rule<Iterator, HostInfo(), ascii::space_type> start;
};

} // end of impl

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class SSHHostListImpl
{
public:
    typedef string::const_iterator Iterator;

public:
    bool init_with_file(string const& file)
    {
        ifstream ifs; 
        ifs.open(file.c_str());
        if (!ifs.is_open())
            throw std::runtime_error("can't open file");

        return init_with_stream(ifs);
    }

    bool init_with_string(string const& str)
    {
        std::stringstream ss(str);
        return init_with_stream(ss);
    }

    bool init_with_stream(std::istream& stream)
    {
        using namespace impl;

        HostInfoParser<Iterator> g;
        string line;
        while (getline(stream, line))
        {
            HostInfo host; 
            if (!parse_line(g, line, host)) 
                return false;

            m_hosts.push_back(host);
        }

        return true;
    }

    auto count() const -> uint32_t
    {
        return m_hosts.size();
    }

    auto nth(int index) -> boost::tuple<string, string, string>
    {
        if (index >= m_hosts.size() || index < 0)
            throw std::runtime_error("wrong index reference");

        auto& h = m_hosts[index];
        return boost::make_tuple(h.host, h.id, h.pw);
    }

private:
    template<typename Iterator>
    auto parse_line(impl::HostInfoParser<Iterator>& g, string const& line, 
            impl::HostInfo& host) -> bool
    {
        Iterator it = line.begin(), 
                end = line.end();
        auto result = phrase_parse(it, end, g, spirit::ascii::space, host); 
        return (result && it == end);
    }

private:
    vector<impl::HostInfo> m_hosts;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SSHHostList::SSHHostList()
{
    _pimpl = new SSHHostListImpl();
}

SSHHostList::~SSHHostList()
{
    delete _pimpl;
}

bool SSHHostList::init_with_file(string const& file)
{
    return _pimpl->init_with_file(file);
}

bool SSHHostList::init_with_string(string const& str)
{
    return _pimpl->init_with_string(str);
}

bool SSHHostList::init_with_stream(std::istream& stream)
{
    return _pimpl->init_with_stream(stream);
}

uint32_t SSHHostList::count() const
{
    return _pimpl->count();
}

boost::tuple<string, string, string>
SSHHostList::nth(int index)
{
    return _pimpl->nth(index);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#if 0

#include <boost/format.hpp>

int main(int argc, char const* argv[])
{
    using namespace boost;

    auto filename = "/Users/yielding/.passwd";
    auto string_  = "host { \"127.0.0.1\", \"y12lding\", \"abc\" }\n"
                    "host { \"127.0.0.2\", \"y12lding\", \"abc\" }\n"
                    "host { \"127.0.0.3\", \"y12lding\", \"abc\" }";
    SSHHostList list;
    list.init_with_string(string_); // h.init_with_file(filename);
    for (int i=0; i<list.count(); i++)
    {
        string host, id, pw; tie(host, id, pw) = list.nth(i);
        cout << str(format("host: %s id : %s pw: %s\n") % host % id % pw);
    }

    return 0;
}
#endif
