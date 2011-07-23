#include <iostream>
#include "boost/tuple/tuple.hpp"

using namespace boost;
using namespace std;

typedef tuple<int, int> div_result;

div_result div_(int a, int b)
{
  return make_tuple(a / b, a % b);
}

int main(int argc, char const* argv[])
{
  int remainder;
  int quoitent;

  tie(remainder, quoitent) = div_(10, 3);

  cout << remainder << "  " << quoitent << endl;
  
  return 0;
}
