#include <iostream>
#include <boost/function.hpp>
#include <boost/tuple/tuple.hpp>

using namespace std;

int main(int argc, char const* argv[])
{
  auto i = 10 + 20;

  // lambda with recursion
  boost::function<int (int)> fact = [&fact](int n)
  {
    return n == 0 ? 1 : n*fact(n-1);
  };

  cout << fact(5);

  return 0;
}


