#include <map>
#include <string>
#include <algorithm>
#include <iostream>

#include <boost/fusion/include/std_pair.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/phoenix/fusion.hpp>

using namespace std;
      namespace phoenix = boost::phoenix;
      using     phoenix::arg_names::arg1;

int main(int argc, char const* argv[])
{
  map<string, int> m;

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

  int const n = 6;
  auto it = find_if(m.cbegin(), m.cend(), phoenix::at_c<1>(arg1) > n);
  if (it != m.cend())
    cout << it->first << endl;

  return 0;
}
