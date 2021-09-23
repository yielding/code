#include <iostream>
#include <vector>
#include <string>
#include <range/v3/view/cycle.hpp>

using namespace ranges;

template <typename F>
void repeat(size_t n, F f)
{
  while(n--) f();
}

int main(int argc, char *argv[])
{
  auto v = {1, 3, 9};

  auto rng = v | views::cycle;
  repeat(10, [&] { std::cout << rng << " "; });
  
  return 0;
}
