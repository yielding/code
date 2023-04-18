#include <iostream>
#include <vector>

#include <range/v3/all.hpp>

using namespace ranges::v3;

int main()
{
  std::vector<int> vi{
    9, 4, 5, 2, 9, 1, 0, 2, 6, 7, 4, 5, 6, 5, 9, 2, 7,
    1, 4, 5, 3, 8, 5, 0, 2, 9, 3, 7, 5, 7, 5, 5, 6, 1,
    4, 3, 1, 8, 4, 0, 7, 8, 8, 2, 6, 5, 3, 4, 5
  };

  vi |= action::sort | action::unique | action::take(4);

  std::cout << views::all(vi) << '\n';

  return 0;
}



