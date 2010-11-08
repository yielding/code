#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <iostream>
#include <string>

using namespace std;

bool is_ipv4address(string const& s)
{
  using namespace boost::spirit;
  using namespace boost::spirit::qi;
  using namespace boost::spirit::ascii;

  string::const_iterator beg = s.begin();
  string::const_iterator end = s.end();

  rule<string::const_iterator> ipv4address;
  rule<string::const_iterator, locals<int> > uint3;

  uint3 
    = uint_ [_a = _1] >> eps(_a >= 0 && _a < 256);
    
  ipv4address 
    = uint3 >> char_('.') >> uint3 >> '.' >> uint3 >> '.' >> uint3;

  return parse(beg, end, ipv4address);
}

int main(int argc, char const *argv[])
{
  char const* addr1 = "255.168.10.123";
  char const* addr2 = "256.168.10.123";

  cout << (is_ipv4address(addr1) ? "true" : "false") << endl;
  cout << (is_ipv4address(addr2) ? "true" : "false") << endl;

  return 0;
}