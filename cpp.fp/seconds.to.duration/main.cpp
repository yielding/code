#include <iostream>
#include <vector>
#include <string>
#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace rg = ranges;
using namespace std;

//
// TODO
// 1. review exclusive_scan(0) of {1, 2, 3}
//    = {0, 0+1, 0+1+2, 0+1+2+3}
// 2. why separate rv::reverse in line 35
//
int main(int argc, char* argv[])
{
  auto sec = 123456;
  auto const times = vector{ 60, 60, 24, 7 };
  auto const names = vector<string> { "s", "min", "h", "d" };

  auto div_pos = times
    | rv::exclusive_scan(sec, divides{})
    | rv::take_while([](int x) { return x > 0; });

  auto mods = rv::zip_with(
      [](int a, int b) { return a % b; },  // 여기서 % 연산이 빛난다.
      div_pos, times);

  auto pairs = rv::zip_with(
      [](int a, auto const& s) { return to_string(a) + s; },
      mods, names)
    | rg::to<vector<string>>;

  cout << rv::all(pairs) << endl;

  auto res = pairs | rv::reverse;

  cout << rv::all(res);

  return 0;
}