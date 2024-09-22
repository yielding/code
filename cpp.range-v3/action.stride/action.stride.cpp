#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace a = ranges::actions;
namespace g = ranges;

using namespace std;

int main(int argc, char *argv[])
{
  using v::ints, v::all, g::copy, g::to, a::stride;

  auto pr = [](auto && r) { cout << all(r) << endl; };

  auto v1 = ints(0, 100) | to<vector>();
  auto v2 = v1 | copy | stride(10);
  pr(v2);

  v2 |= stride( 4); pr(v2);
  v2 |= stride( 2); pr(v2);
  v2 |= stride(10); pr(v2);
  
  return 0;
}