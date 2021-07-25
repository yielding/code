#include <vector>
#include <ranges>
#include <iostream>

using namespace std;

int main()
{
  auto ints = { 0, 1, 2, 3, 4, 5 };

  auto even   = [] (int i) { return 0 == i % 2; };
  auto square = [] (int i) { return i * i; };

  for (int i: ints | views::filter(even) | views::transform(square)) 
    cout << i << ' ';

  return 0;
}

