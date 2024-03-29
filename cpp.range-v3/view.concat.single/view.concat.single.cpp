#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace rg = ranges;

using std::vector,
      std::cout, std::endl;

int main(int argc, char* argv[])
{
  auto const v1 = vector{ 2,  3,  7};
  auto const v2 = vector{11, 17, 21};
  cout << rv::concat(v1, v2) << endl;

  auto const v3 = rv::concat(rv::single(10), v2);
  cout << v3 << endl;

  auto const v4 = vector<vector<int>> {{1, 2}, {7}, {3, 5}};
  auto rng = v4 | rv::join;
  cout << rng << endl;

  return 0;
}
