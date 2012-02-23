#include <iostream>
#include <string>

#include <boost/phoenix/core.hpp>

using namespace std;

int main(int argc, char const* argv[])
{
  using boost::phoenix::ref;
  using namespace boost::phoenix::arg_names;

  int    i = 10;
  string s = "Hello";

  cout << ref(i)() << endl;
  cout << ref(s)() << endl;
  cout << arg2(i, s) << endl;

  return 0;
}
