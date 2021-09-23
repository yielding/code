#include <iostream>
#include <vector>
#include <string>
#include <range/v3/view/cycle.hpp>

using namespace ranges;

int main(int argc, char *argv[])
{
  auto v = {1, 3, 9};

  auto rng = v | views::cycle;

  int c=0;
  for (auto i: rng) 
  {
    std::cout << i;
    if (c++ == 10) break;
  }
  
  return 0;
}
