#include <vector>
#include <iostream>

#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>

using namespace ranges::v3::view;

int main()
{
  auto ints = { 0, 1, 2, 3, 4, 5 };
  auto even   = [] (int i) { return 0 == i % 2; };
  auto square = [] (int i) { return i * i; };
  auto rng    = ints | filter(even) | transform(square);

  for (auto i: rng) std::cout << i << ' ' << std::endl;

  std::cout << all(rng);

  return 0;
}
