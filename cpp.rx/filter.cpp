#include <iostream>
#include <string>
#include <vector>
#include "rxcpp/rx.hpp"

template <typename Value>
void p(Value v)
{
  std::cout << v << std::endl;
}

int main(int argc, char *argv[])
{
  auto values = rxcpp::observable<>::range(1, 10)
    .filter([](int v) { return v % 2; });

  values.subscribe(
      [](int v) { p(v); },
      []()      { p("completed"); }
  );
  
  return 0;
}
