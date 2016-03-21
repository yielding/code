#include <boost/spirit/home/x3.hpp>

#include <string>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  namespace x3 = boost::spirit::x3;

  auto in = "-1234.56"s;
  auto a  = 0.;
  x3::parse(in.begin(), in.end(), x3::double_, a);

  std::cout << a << std::endl;
  
  return 0;
}
