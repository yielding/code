#include <iostream>
#include <boost/function.hpp>

using namespace std;

int main(int argc, char const* argv[])
{
  // lambda with recursion
  boost::function<int (int)> fact = [&fact](int n) {
    return n == 0 ? 1 : n*fact(n-1);
  };

  cout << fact(5);

  return 0;
}
