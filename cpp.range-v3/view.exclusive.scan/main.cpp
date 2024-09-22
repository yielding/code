#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;
using strings = vector<string>;

//
// TODO
// 1. review exclusive_scan(0) of {1, 2, 3}
//    = {0, 0+1, 0+1+2, 0+1+2+3}
// 2. why separate v::reverse in line 35
//
int main(int argc, char* argv[])
{
  using v::exclusive_scan, v::take_while, v::zip_with, v::reverse, g::to;

  auto sec = 123456;
  auto times = vector{ 60, 60, 24, 7 };
  auto names = vector{ "s"s, "min"s, "h"s, "d"s };

  auto div_pos 
    = times
    | exclusive_scan(sec, divides{})
    | take_while([](int x) { return x > 0; });

  auto mods = zip_with(
      [](int a, int b) { return a % b; },  // 여기서 % 연산이 빛난다.
      div_pos, times);

  auto pairs 
    = zip_with([](int a, auto& s) { return to_string(a) + s; }, mods, names)
    | to<strings>;

  cout << v::all(pairs | reverse);

  return 0;
}