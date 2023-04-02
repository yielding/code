#include <vector>
#include <iostream>

#include <range/v3/view/filter.hpp>
#include <range/v3/view/transform.hpp>

namespace view = ranges::views;

using namespace std;

int main()
{
  auto ints = { 0, 1, 2, 3, 4, 5 };
  auto even   = [] (int i) { return 0 == i % 2; };
  auto square = [] (int i) { return i * i; };
  auto rng    = ints | view::filter(even) | view::transform(square);

  for (auto i: rng) cout << i << ' ' << endl;

  cout << view::all(rng);

  return 0;
}
