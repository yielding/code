#include <boost/algorithm/string.hpp>
#include <boost/format.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/include/io.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/tuple/tuple.hpp>

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdexcept>

using std::string;

namespace utility { namespace comm { namespace ssh {
    struct HostInfo { string host, id, pw; };
}
}
}

////////////////////////////////////////////////////////////////////////////////
//
// NOTICE: It seems that it should be outside of namespace
//
////////////////////////////////////////////////////////////////////////////////
BOOST_FUSION_ADAPT_STRUCT(
    utility::comm::ssh::HostInfo,
    (string, host) (string, id) (string, pw)
)

namespace utility { namespace comm { namespace ssh {
////////////////////////////////////////////////////////////////////////////////
//
// 
//
////////////////////////////////////////////////////////////////////////////////
namespace    qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

template <typename Iterator>
struct HostInfoParser : qi::grammar<Iterator, HostInfo(), ascii::space_type>
{
    HostInfoParser()
        : HostInfoParser::base_type(start)
    {
        using namespace qi;
        qs    %= lexeme['"' >> +(char_ - '"') >> '"'];
        start %= lit("host") >> '{' >> qs >> ',' >> qs >> ',' >> qs >> '}';
    }

    qi::rule<Iterator, string(), ascii::space_type> qs;
    qi::rule<Iterator, HostInfo(), ascii::space_type> start;
};

} // end of ssh
} // end of comm
} // end of utility

class SSHHosts
{
public:
    typedef string::const_iterator Iterator;

public:
    bool init_with_file(string&& file)
    {
        std::ifstream ifs; 
        ifs.open(file.c_str());
        if (!ifs.is_open())
            throw std::runtime_error("can't open file");

        return init_with_stream(ifs);
    }

    bool init_with_string(string&& str)
    {
        std::stringstream ss(str);
        return init_with_stream(ss);
    }

    bool init_with_stream(std::istream& stream)
    {
        using namespace utility::comm::ssh;

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
            throw std::runtime_error("woring index reference");

        auto& h = m_hosts[index];
        return boost::make_tuple(h.host, h.id, h.pw);
    }

private:
    template<typename Iterator>
    auto parse_line(utility::comm::ssh::HostInfoParser<Iterator>& g, string const& line, 
            utility::comm::ssh::HostInfo& host) -> bool
    {
        Iterator it = line.begin(), 
                end = line.end();
        auto result = phrase_parse(it, end, g, boost::spirit::ascii::space, host); 
        return (result && it == end);
    }

private:
    std::vector<utility::comm::ssh::HostInfo> m_hosts;
};

int main(int argc, char const* argv[])
{
    using namespace std;
    using namespace boost;

    auto filename = "/Users/yielding/.passwd";
    auto string_  = "host { \"127.0.0.1\", \"y12lding\", \"abc\" }\n"
                    "host { \"127.0.0.2\", \"y12lding\", \"abc\" }\n"
                    "host { \"127.0.0.3\", \"y12lding\", \"abc\" }";
    SSHHosts h;
    h.init_with_string(string_); // h.init_with_file(filename);
    for (int i=0; i<h.count(); i++)
    {
        string host, id, pw; tie(host, id, pw) = h.nth(i);
        cout << str(format("host: %s id : %s pw: %s\n") % host % id % pw);
    }

    return 0;
}
