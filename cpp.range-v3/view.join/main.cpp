#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

auto main(int argc, char* argv[]) -> int
{
  using v::join, v::all;

  auto const v = vector<vector<int>>{{1, 2}, {7}, {3, 5}}; 
  auto rng = v | join; // [1,2,7,3,5]
  cout << all(rng);

  return 0;
}
