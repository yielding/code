#include <iostream>
#include <vector>
#include "scope_guard.hpp"

using namespace std;

namespace io::test 
{
  // target compile options
  // target compile definitions
}

int main(int argc, char *argv[])
{
  vector<int> v = { 1, 2, 3, 4, 5 };

  auto guard = sg::make_scope_guard(
      []{ cout << "hi"; });

  for (auto i : v) cout << i << endl;

  guard.dismiss();

  return 0;
}
