#include <boost/phoenix/core.hpp>
#include <boost/phoenix/function.hpp>

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

struct is_odd_
{
  typedef bool result_type;

  template <typename Arg> 
  bool operator()(Arg arg1) const 
  {
    return arg1 % 2 == 1;
  }
};

boost::phoenix::function<is_odd_> is_odd;

int main(int argc, char const* argv[])
{
  using namespace boost::phoenix::arg_names;

  vector<int> c = { 2, 10, 4, 5, 1, 6, 8, 3, 9, 7 };

  auto it = find_if(c.begin(), c.end(), is_odd(arg1));
  if (it != c.end())
      cout << *it << endl;
  
  return 0;
}
