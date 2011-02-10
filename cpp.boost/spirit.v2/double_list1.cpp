#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>

#include <string>
#include <vector>
#include <iostream>

namespace client
{
  namespace qi    = boost::spirit::qi;
  namespace ascii = boost::spirit::ascii;
  namespace phoenix = boost::phoenix;

  template <typename It> 
  bool parse_numbers(It first, It last, std::vector<double>& v)
  {
    using qi::double_;
    using qi::phrase_parse;
    using qi::_1;
    using ascii::space;
    using phoenix::push_back;
    using phoenix::ref;

    bool r = phrase_parse(first, last, 
        (
           double_[push_back(phoenix::ref(v), _1)] % ','
        ),
        space);

      return (first != last) ? false : r;
    }
}

int main(int argc, char const* argv[])
{
  std::string s = "1, 2, 3.3, 4, ";
  std::vector<double> v; 
  client::parse_numbers(s.begin(), s.end(), v);

  std::cout << std::accumulate(v.begin(), v.end(), 0.0);

  return 0;
}
