#include <boost/spirit/include/qi.hpp>

#include <iostream>
#include <numeric>

using namespace std;

namespace client
{
  namespace qi    = boost::spirit::qi;
  namespace ascii = boost::spirit::ascii;

  template <typename It> 
  bool parse_numbers(It first, It last, vector<double>& v)
  {
    using qi::double_;
    using qi::phrase_parse;

    // phrase를 끊는 단위가 space
    bool r = phrase_parse(first, last, (double_ % ','), ascii::space, v);

    return (first != last) ? false : r;
  }
}

int main(int argc, char const* argv[])
{
  string s = "1 , 2,                   3.3, 4, 5.56";
  vector<double> v; 
  client::parse_numbers(s.begin(), s.end(), v);

  cout << accumulate(v.begin(), v.end(), 0.0);

  return 0;
}
