#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

auto main() -> int
{
  using v::join;

  auto bits = { "https:"sv, "//"sv, "cppreference"sv, "."sv, "com"sv };

  for (auto c : bits | join) cout << c << " ";
  cout << endl;

  auto v = vector<vector<int>> { {1,2}, {3,4,5}, {6}, {7,8,9} };

  for (auto e : v | join) cout << e << ' ';
  cout << endl;

  return 0;
}
