#include <iostream>
#include <boost/phoenix/core.hpp>

using namespace std;

template <typename F> 
void pr(F f)
{
  cout << f() << endl;
}

int main(int argc, char const* argv[])
{
  using boost::phoenix::val;
  int x;

  cout << val("Hello")() << endl;
  pr(val(3));

  return 0;
}
