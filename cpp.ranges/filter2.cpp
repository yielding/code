#include <vector>
#include <iostream>

#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>

using namespace ranges;

int main()
{
  auto ints = { 0, 1, 2, 3, 4, 5 };

  auto even   = [] (int i) { return 0 == i % 2; };
  auto square = [] (int i) { return i * i; };

  for (int i: ints | views::filter(even) | views::transform(square)) 
    std::cout << i << ' ';

  return 0;
}
