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
<<<<<<< HEAD

  int c=0;
  for (auto i: rng) 
  {
    std::cout << i;
    if (c++ == 10) break;
  }
=======
  repeat(10, [&] { std::cout << rng << " "; });
>>>>>>> 33070fd591db709987dfed7d6fdb064f78488a54
  
  return 0;
}
