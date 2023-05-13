#include <iostream>
#include <vector>
#include <string>
#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace rg = ranges;
using namespace std;

// TODO
// 1. review exclusive_scan
// 2. why separate rv::reverse in line 35
int main(int argc, char* argv[])
{
  auto sec = 123456;
  auto const v = vector{60, 60, 24, 7};
  auto const names = vector<string> {
    "s", "min", "h", "d" };

  auto div_pos = v 
    | rv::exclusive_scan(sec, divides{})
    | rv::take_while([](int x) { return x > 0; })
    ;

  auto mods = rv::zip_with(
      [](int a, int b) { return a % b; },
      div_pos, v);

  auto pairs = rv::zip_with(
      [](int a, auto const& s) { return to_string(a) + s; },
      mods, names)
    | rg::to<vector<string>>
    ;

  auto res = pairs | rv::reverse;

  cout << rv::all(res);

  return 0;
}