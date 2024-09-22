#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char *argv[])
{
  using v::intersperse, v::all, g::to;

  auto s1 = "London"s;
  auto s = s1 | intersperse('_') | to<string>();
  cout << all(s) << endl;

  auto v = { 1, 2, 3, 4 };
  auto r = v | intersperse(0);
  cout << all(r) << endl;

  return 0;
}
