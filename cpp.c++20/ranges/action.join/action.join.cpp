#include <iostream>
#include <ranges>
#include <string_view>
#include <vector>

using namespace std;
using namespace literals;

int main()
{
  auto bits = { "https:"sv, "//"sv, "cppreference"sv, "."sv, "com"sv };

  for (auto c : bits | views::join) 
    cout << c << " ";

  cout << '\n';

  vector<vector<int>> v{ {1,2}, {3,4,5}, {6}, {7,8,9} };
  auto jv = ranges::join_view(v);

  for (int const e : jv) 
    cout << e << ' ';

  cout << '\n';

  return 0;
}
