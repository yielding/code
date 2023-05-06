#include <iostream>
#include <vector>
#include <string>
#include <format>
#include <cassert>

#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::view;

using std::string, std::format, std::vector;

int main(int argc, char* argv[])
{
  auto org = vector{1, 1, 1, 0};
  auto reversed = org | rv::reverse;
  auto base = rv::iota(0, rg::distance(org)) 
            | rv::transform([](int x) { return 1 << x; });

  std::cout << rg::inner_product(reversed, base, 0);

  return 0;
}