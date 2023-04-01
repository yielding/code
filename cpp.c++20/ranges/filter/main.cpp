#include <iostream>
#include <ranges>

using namespace std;
using namespace views;

int main()
{
  auto even   = [](int i) { return 0 == i % 2; };
  auto square = [](int i) { return i * i; };

  auto rng = iota(0, 6) | filter(even) | transform(square);
  
  for (int i : rng)
    cout << i << ' ';

  return 0;
}