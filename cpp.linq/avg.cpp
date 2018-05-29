#include "cpplinq.hpp"
#include <vector>
#include <iostream>

using namespace cpplinq;
using namespace std;

int main(int argc, char *argv[])
{
  vector<int> v0 = { 1, 2, 3, 4, 5 };

  auto res = from(v0) >> avg();

  cout << res << endl;

  return 0;
}
