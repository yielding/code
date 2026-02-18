#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto main(int argc, char* argv[]) -> int
{
  using v::iota, v::cycle, v::drop, v::take, v::transform, g::accumulate;

  auto rc  = iota(1, 10) | cycle;
  auto rng = iota(1)
    | transform([rc](int x) -> long long {
        auto rd = rc | drop(x*(x-1)/2) | take(x);
        return accumulate(rd, 0LL, [](auto a, auto b) -> auto { return a*10 + b; });
      });

  cout << v::all(rng | take(10)) << endl;

  return 0;
}
