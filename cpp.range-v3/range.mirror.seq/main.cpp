#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto main(int argc, char* argv[]) -> int
{
  using v::iota, v::all, v::reverse, v::drop, v::concat, g::accumulate;

  cout << all(iota(1, 10)) << endl;

  auto rng = iota(1, 10)
    | v::transform([](int x) -> auto {
        auto r1 = v::closed_iota(1, x);
        auto r2 = r1 | drop(1) | reverse;
        auto r3 = concat(r2, r1);
        return accumulate(r3, 0LL, [](auto s, auto i) -> auto { return s * 10 + i; });
      });

  cout << all(rng) << endl;

  return 0;
}