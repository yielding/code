#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  auto rng = v::iota(1, 10)
    | v::transform([](int x) {
        auto r1 = v::closed_iota(1, x);
        auto r2 = r1 | v::drop(1) | v::reverse;
        auto r3 = v::concat(r2, r1);
        return g::accumulate(r3, 0LL, [](auto s, auto i) { return s * 10 + i;});
      });

  cout << v::all(rng) << endl;

  return 0;
}