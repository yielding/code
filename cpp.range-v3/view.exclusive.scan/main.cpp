#include <iostream>
#include <vector>
#include <string>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;
using namespace std;

//
// TODO
// 1. review exclusive_scan(0) of {1, 2, 3}
//    = {0, 0+1, 0+1+2, 0+1+2+3}
// 2. why separate v::reverse in line 35
//
int main(int argc, char* argv[])
{
  auto sec = 123456;
  auto const times = vector{ 60, 60, 24, 7 };
  auto const names = vector<string> { "s", "min", "h", "d" };

  auto div_pos = times
    | v::exclusive_scan(sec, divides{})
    | v::take_while([](int x) { return x > 0; });

  auto mods = v::zip_with(
      [](int a, int b) { return a % b; },  // 여기서 % 연산이 빛난다.
      div_pos, times);

  auto pairs = v::zip_with(
      [](int a, auto const& s) { return to_string(a) + s; },
      mods, names)
    | g::to<vector<string>>;

  cout << v::all(pairs) << endl;

  auto res = pairs | v::reverse;

  cout << v::all(res);

  return 0;
}