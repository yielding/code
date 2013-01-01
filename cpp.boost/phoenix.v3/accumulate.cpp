#include <iostream>
#include <algorithm>
#include <vector>

#include <boost/phoenix/core.hpp>
#include <boost/phoenix/operator.hpp>

using namespace std;
using namespace boost::phoenix::arg_names;

int main(int argc, char const* argv[])
{
  vector<int> v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

  cout << accumulate(v.begin(), v.end(), 0, _1 + _2);
  
  return 0;
}
