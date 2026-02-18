#include <array>
#include <random>
#include <print>
#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

void pr(const auto& a) 
{
  for (const auto e : a) print("{} ", e);

  println("");
}

auto main() -> int
{
  array a{'A', 'B', 'C', 'D', 'E', 'F'};
  pr(a);

  random_device rd;
  mt19937 gen{rd()};

  for (int i{}; i != 3; ++i) 
  {
    actions::shuffle(a, gen);
    pr(a);
  }

  return 0;
}