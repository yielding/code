#include <iostream>
#include <range/v3/all.hpp>

namespace rv = ranges::view;

int main(int argc, char* argv[])
{
  auto fib = [p=std::pair{0, 1}]() mutable {
    auto [x0, y0] = p;
    p = {y0, x0 + y0};
    return x0;
  };

  std::cout << (rv::generate(fib)| rv::take(10));

  return 0;
}