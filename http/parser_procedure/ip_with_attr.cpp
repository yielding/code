#include <boost/detail/lightweight_test.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_stl.hpp> 
#include <boost/fusion/include/adapt_struct.hpp>

#include <iostream>
#include <string>

using namespace std;

struct ip_
{
  unsigned u1;
  unsigned u2;
  unsigned u3;
  unsigned u4;
};

std::ostream& operator<<(std::ostream& out, ip_ const& ip)
{
  out << ip.u1 << "." << ip.u2 << "." << ip.u3 << "." << ip.u4 << std::endl;

  return out;
}

BOOST_FUSION_ADAPT_STRUCT(ip_,
  (unsigned, u1)
  (unsigned, u2)
  (unsigned, u3)
  (unsigned, u4)
)

bool is_ipv4address(string const& s)
{
  using namespace boost::spirit;
  using namespace boost::spirit::qi;
  using namespace boost::spirit::ascii;

  string::const_iterator beg = s.begin();
  string::const_iterator end = s.end();

  rule<string::const_iterator, ip_()> ipv4address;
  rule<string::const_iterator, unsigned(), locals<int> > uint3;

  uint3
    %= uint_ [_a = _1] >> eps(_a >= 0 && _a < 256);

  ipv4address
    %= uint3 >> '.' >> uint3 >> '.' >> uint3 >> '.' >> uint3;

  ip_ ip;
  bool res = parse(beg, end, ipv4address, ip);
  if (res) cout << ip;
  return res;
}

int main(int argc, char const *argv[])
{
  char const* addr1 = "255.168.10.123";
  char const* addr2 = "256.168.10.123";
  char const* addr3 = "25 .168.10.123";

  BOOST_TEST(is_ipv4address(addr1));
  BOOST_TEST(!is_ipv4address(addr2));
  BOOST_TEST(!is_ipv4address(addr3));

  return boost::report_errors();
}
