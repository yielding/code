#include <iostream>
#include <string>
#include <list>
#include <vector>
#include <boost/format.hpp>
#include "scope_guard.hpp"

using namespace std;

namespace io::test 
{
  // target compile options
  // target compile definitions
}

int main(int argc, char *argv[])
{
  list<int> l = { 1, 2, 3, 4 };
  vector<int> v = { 1, 2, 3, 4, 5 };
  string s;

  auto guard = sg::make_scope_guard([]{ cout << "hi"; });

  for (auto i : v) cout << i << endl;

  guard.dismiss();

  return 0;
}
