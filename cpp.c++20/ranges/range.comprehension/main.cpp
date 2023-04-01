#include <iostream>
#include <vector>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/for_each.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/repeat_n.hpp>

using namespace ranges::views;
using namespace ranges;

int main()
{
  auto fn = [](int i) { return yield_from(repeat_n(i, i)); };
  auto vi = for_each(ints(1, 6), fn) 
              | to<std::vector>();

  std::cout << all(vi) << '\n';

  return 0;
}