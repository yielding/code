#include <iostream>
#include <algorithm>
#include <vector>

#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>

using namespace std;
using namespace boost::phoenix::arg_names;

int main(int argc, char const* argv[])
{
  using boost::phoenix::ref;

  int x;
  (arg1 = arg2 + (3 * arg3)) (ref(x), 3, 4);

  cout << x;
  
  return 0;
}
