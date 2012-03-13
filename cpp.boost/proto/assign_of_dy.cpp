#include <iostream>
#include <map>
#include <cassert>
#include <boost/assign/list_of.hpp>

using namespace std;
using namespace boost::assign;

int main(int argc, char const* argv[])
{
  map<int, int> next = map_list_of(1, 2)(3, 4);

  assert(next[1] == 2);
  return 0;
}
