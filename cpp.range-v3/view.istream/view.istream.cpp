#include <iostream>
#include <sstream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char *argv[])
{
  using v::take, v::all, g::istream;

  auto s0 = stringstream{"a b c"};
  auto r0 = istream<char>(s0) | take(2);
  cout << all(r0) << endl;

  auto s1 = stringstream{"1 23 456"};
  auto r1 = istream<int>(s1) | take(2);
  cout << all(r1) << endl;

  return 0;
}
