#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;
namespace a = ranges::actions;

using namespace std;

int main(int argc, char* argv[])
{
  auto vec = vector{1, 3, 6};
  auto val = g::accumulate(vec, 0);

  // action - O(n)
  vec |= a::transform([](int x) { return 2 * x; });
  cout << v::all(vec);

  //  view - O(1)
  auto rng = vec | v::transform([](int x) { return 2 * x; });
  cout << v::all(rng);

  return 0;
}
