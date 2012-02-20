#include <iostream>
#include <vector>
#include <algorithm>
#include <boost/phoenix/statement.hpp>
#include <boost/phoenix/operator.hpp>
#include <boost/phoenix/core.hpp>

using namespace std;

int main()
{
  using boost::phoenix::if_;
  using boost::phoenix::arg_names::arg1;

  vector<int> v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

  cout << dec;
  for_each(v.begin(), v.end(), if_(arg1 > 5) [ cout << arg1 << ", " ]);
  cout << endl;

  return 0;
}

