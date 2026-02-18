#include <iostream>

#include <range/v3/all.hpp>

using namespace ranges::views;

auto main() -> int
{
  auto ints   = { 0, 1, 2, 3, 4, 5 };
  auto even   = [](int i) -> bool { return 0 == i % 2; };
  auto square = [](int i) -> int  { return i * i; };
  auto rng    = ints | filter(even) | transform(square);

  std::cout << all(rng);

  return 0;
}
