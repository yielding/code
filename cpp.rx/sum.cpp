#include <iostream>

#include "rxcpp/rx.hpp"

int main(int argc, char *argv[])
{
  auto v = rxcpp::observable<>::range(1, 10).sum();

  v.subscribe(
    [](int v) { std::cout << v << std::endl; },
    []{}
  );

  return 0;
}
