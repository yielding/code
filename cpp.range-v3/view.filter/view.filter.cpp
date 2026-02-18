#include <iostream>
#include <range/v3/all.hpp>

using namespace ranges::views;

auto main() -> int
{
  auto even   = [](int i) -> bool { return i % 2 == 0; };
  auto square = [](int i) -> int  { return i * i; };

  auto rng = iota(0, 6) | filter(even) | transform(square);
  
  for (int i : rng)
    std::cout << i << ' ';

  return 0;
}