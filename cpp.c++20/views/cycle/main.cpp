#include <iostream>
#include <vector>
#include <string>
#include <range/v3/view/cycle.hpp>

using namespace ranges;
using namespace std;

template <typename F>
void repeat(size_t n, F f)
{
  while (n--) f();
}

int main(int argc, char *argv[])
{
  auto v = {1, 3, 9};

  auto rng = v | views::cycle;

  int c=0;
  for (auto i: rng) 
  {
    cout << i;
    if (c++ == 10) break;
  }

  repeat(10, [&] { cout << rng << " "; });
  
  return 0;
}