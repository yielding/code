#include <map>
#include <string>
#include <algorithm>
#include <iostream>

#include <boost/fusion/include/std_pair.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/phoenix/fusion.hpp>

namespace phoenix = boost::phoenix;
using     phoenix::arg_names::arg1;

int main(int argc, char const* argv[])
{
  std::map<std::string, int> m;

  m["foo"]    = 1;
  m["bar"]    = 2;
  m["baz"]    = 3;
  m["qux"]    = 4;
  m["corge"]  = 6;
  m["grault"] = 7;
  m["garply"] = 8;
  m["waldo"]  = 9;
  m["fred"]   = 10;
  m["plugh"]  = 11;
  m["xyzzy"]  = 12;
  m["thud"]   = 13;

  int n = 4;
  auto it = find_if(m.cbegin(), m.cend(), phoenix::at_c<0>(arg1).length() > n);
  if (it != m.cend())
    std::cout << it->first << std::endl;

  return 0;
}
