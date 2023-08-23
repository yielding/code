#include <iostream>
#include <vector>
#include <algorithm>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto v = vector { 8, 7, 3 };

  auto r0 = v | v::filter([](int x) { return x > 4; });
  auto s0 = accumulate(r0.begin(), r0.end(), 0);

  cout << s0 << endl;

  // NOTICE
  // common_range provides 
  // ranges::begin, ranges::end for the old STL algorithm
  //
  auto r1 = v | v::take_while([](int x) { return x > 4; })
              | v::common;

  auto s1 = accumulate(r1.begin(), r1.end(), 0);
  cout << s1 << endl;

  return 0;
}
