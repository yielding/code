#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::view;

using namespace std;

int main(int argc, char* argv[])
{
  using v::generate, v::take;

  auto fib = [p=pair{0, 1}]() mutable {
    auto [x0, y0] = p;
    p = {y0, x0 + y0};
    return x0;
  };

  cout << (generate(fib)| take(10));

  return 0;
}