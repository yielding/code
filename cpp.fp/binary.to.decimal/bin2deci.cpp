#include <iostream>
#include <string>
#include <cassert>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int agc, char* agv[])
{
  auto og = vector{1, 1, 1, 0};
  auto reversed = og | v::reverse;
  auto base = v::iota(0, g::distance(og)) 
            | v::transform([](int x) { return 1 << x; });

  cout << g::inner_product(reversed, base, 0);

  return 0;
}