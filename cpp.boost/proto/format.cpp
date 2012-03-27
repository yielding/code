#include <map>
#include <string>
#include <iostream>
#include <boost/proto/proto.hpp>
#include <boost/xpressive/xpressive.hpp>
#include <boost/xpressive/regex_actions.hpp>

struct map_ {};
boost::proto::terminal<map_>::type map = {};

typedef std::map<std::string, std::string> string_map;

// Recursive function used to fill the map
template<class Expr>
void fill_map(Expr const & expr, string_map & subs)
{
    using boost::proto::value;      // read a value from a terminal
    using boost::proto::child_c;    // get the Nth child of a non-terminal

    subs[ value(child_c<1>(expr)) ] = value(child_c<2>(expr));
    fill_map(child_c<0>(expr), subs);
}

// The 'map' terminal ends the recursion
void fill_map(boost::proto::terminal<map_>::type const &, string_map &)
{}

// The old format API that accepts a map of string substitutions
// REMARK: using another DSEL to define a DSEL.
// This is one of the eminent features of proto.
std::string format(std::string fmt, string_map & subs)
{
    namespace xp = boost::xpressive;
    using namespace xp;
    sregex const rx = '{'>> (s1= +_w)>> '}';        // like "{(\\w+)}"
    return regex_replace(fmt, rx, xp::ref(subs)[s1]);
}

// The new format API that forwards to the old one
template<class Expr>
std::string format(std::string fmt, Expr const & expr)
{
    string_map subs;
    fill_map(expr, subs);
    return format(fmt, subs);
}

int main()
{
    std::cout << format("There are more things in {place}, Horatio, "
                        "than are dreamt of in your {thing}.\n"
                        , map("place", "heaven and earth")
                             ("thing", "philosophy"));
    return 0;
}
