#include <iostream>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;
using namespace std;

int main(int argc, char* argv[])
{
  auto const s = string{"))(()("};

  auto r_sum = s 
    | rv::transform([](char c) { return c == '(' ? 1 : -1; })
    | rv::partial_sum;

  auto min_pos = rg::distance(begin(r_sum), rg::min_element(r_sum));

  auto after_min = s 
    | rv::cycle 
    | rv::drop(min_pos + 1);

  auto rng = after_min 
    | rv::take(rg::distance(s));

  cout << rv::all(rng);

  return 0;
}