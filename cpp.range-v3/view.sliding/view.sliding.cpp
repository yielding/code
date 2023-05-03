#include <iostream>
#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace rg = ranges;

using std::cout, std::endl;

int main(int argc, char* argv[])
{
  auto r1 = rv::closed_iota(1, 6);
  auto r2 = r1 | rv::sliding(3);
  auto c2 = r1 | rv::chunk(2);

  cout << rv::all(r2) << endl;
  cout << rv::all(c2) << endl;

  return 0;
}