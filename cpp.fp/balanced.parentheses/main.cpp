#include <range/v3/all.hpp>

#include <iostream>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

auto main(int agc, char* agv[]) -> int
{
  using g::distance, g::min_element;
  using v::transform, v::partial_sum, v::take, v::drop, v::cycle;

  auto const s = string{"))(()("};

  auto r_sum = s | transform([](char c) -> auto { return c == '(' ? 1 : -1; }) 
                 | partial_sum;

  auto min_pos = g::distance(begin(r_sum), min_element(r_sum));

  auto after_min = s | cycle | drop(min_pos + 1);

  auto rng = after_min | take(distance(s));

  cout << v::all(rng);

  return 0;
}