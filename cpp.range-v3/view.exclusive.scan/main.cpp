#include <print>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;
using strings = vector<string>;

void basics()
{
  using v::exclusive_scan;

  // REMARK
  // 1. 주어진 입력에서 원소를 하나씩 증가시키면서 리스트를 만든다. 
  //    (마지막은 빠진다. inclusive_scan은 포함)
  //    [init] [init, 4], [init, 4, 3], [init, 4, 3, 5]
  // 
  // 2. 기본 연산은 plus{}
  // 3. 초기값은 결과에 포함
  // 4. 각 리스트의 원소에 연산 적용
  // 5. 결과를 리스트로 만든다. (monad)

  auto const v = {4, 3, 5, 6};

  auto rng = v | exclusive_scan(0); // [0,0+4,0+4+3,0+4+3+5]...[0,4,7,12]

  for (const auto& value : rng) print("{} ", value); // 0 4 7 12
  println();
}

void applied()
{
  //
  // TODO
  // 1. review exclusive_scan(0) of {1, 2, 3}
  //    = {0, 0+1, 0+1+2, 0+1+2+3}
  // 2. why separate v::reverse in line 38
  //
  using v::exclusive_scan, v::take_while, v::zip_with, v::reverse, g::to;

  auto seconds = 123456;
  auto times = { 60, 60, 24, 7 };

  auto div_pos 
    = times
    | exclusive_scan(seconds, divides{});
    //| take_while([](int x) { return x > 0; });

  // cout << v::all(div_pos) << endl; // [123456,2057,34,1]

  auto names = { "s"s, "min"s, "h"s, "d"s };
  auto mods = zip_with(
      [](int a, int b) -> int { return a % b; },  // 여기서 % 연산이 빛난다.
      div_pos, times);

  // cout << v::all(mods) << endl; // [36,17,10,1]
  auto pairs 
    = zip_with([](int a, auto& s) -> auto { return to_string(a) + s; }, mods, names)
    | to<strings>;

  println("{}", v::all(pairs | reverse)); // [36s,17min,10h,1d]
}

auto main(int argc, char* argv[]) -> int
{
  basics();
  applied();

  return 0;
}
