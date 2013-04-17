#include <algorithm>
#include <map>
#include <string>
#include <iostream>
#include <utility>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/phoenix/fusion.hpp>

using namespace std;
namespace phx = boost::phoenix;
    using phx::arg_names::arg1;

int main()
{
  map<string, int> m = 
  {
    {"foo",    1}, {"bar",    2}, {"baz"   , 3},
    {"qux",    4}, {"quux",   5}, {"corge" , 6},
    {"grault", 7}, {"garply", 8}, {"waldo" , 9},
    {"fred",  10}, {"plugh", 11}, {"xyzzy" , 12},
    {"thud",  13}
  };

  int const n = 6;
  auto it = find_if(m.cbegin(), m.cend(), phx::at_c<1>(arg1) > n);
  if (it != m.cend())
      cout << it->first << '\n'; // prints "fred"

  auto comp = [&n](pair<string, int> const& p) { return p.second > n; };

  auto it2 = find_if(m.cbegin(), m.cend(), comp);
  if (it2 != m.cend())
      cout << it2->first << endl;
}

