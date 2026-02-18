#include <iostream>

#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

auto main(int argc, char* argv[]) -> int
{
  using v::adjacent_remove_if, v::all;

  auto v = {1, 2, 3, 3, 4, 4};
  auto r = v | adjacent_remove_if(equal_to{});

  cout << all(r) << endl; // [1, 2, 3, 4]

  return 0;
}