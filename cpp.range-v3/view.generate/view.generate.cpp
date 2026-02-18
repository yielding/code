#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

auto main(int argc, char* argv[]) -> int
{
  using v::generate, v::take;

  auto fib = [p=pair{0, 1}]() mutable -> auto {
    auto [x0, y0] = p;
    p = {y0, x0 + y0};
    return x0;
  };

  cout << (generate(fib) | take(10));

  return 0;
}