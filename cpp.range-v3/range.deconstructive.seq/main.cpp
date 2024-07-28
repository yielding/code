#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;
using namespace std;

int main(int argc, char* argv[])
{
  auto rc  = v::iota(1, 10) | v::cycle;
  auto rng = v::iota(1)
    | v::transform([rc](int x) {
        auto rd = rc | v::drop(x*(x-1)/2)
                     | v::take(x);
        return g::accumulate(rd, 0LL,
                             [](auto a, auto b) { return a*10 + b; });
        });

  cout << v::all(rng | v::take(10)) << endl;

  return 0;
}
