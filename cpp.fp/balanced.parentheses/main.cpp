#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int agc, char* agv[])
{
  using v::transform, v::partial_sum, v::take, v::drop, v::cycle;
  using g::distance, g::min_element;

  auto const s = string{"))(()("};

  auto r_sum = s 
    | transform([](char c) { return c == '(' ? 1 : -1; })
    | partial_sum;

  auto min_pos = g::distance(begin(r_sum), min_element(r_sum));

  auto after_min = s 
    | cycle 
    | drop(min_pos + 1);

  auto rng = after_min 
    | take(distance(s));

  cout << v::all(rng);

  return 0;
}