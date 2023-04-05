#include <iostream>
#include <vector>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/for_each.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/repeat_n.hpp>

using namespace ranges::v3::views;
using namespace ranges::v3;

using std::vector, std::cout;

int main()
{
  auto fn = [](int i) { return yield_from(repeat_n(i, i)); };
  auto vi = for_each(ints(1, 6), fn) | to<vector>();

  cout << all(vi) << '\n';

  return 0;
}