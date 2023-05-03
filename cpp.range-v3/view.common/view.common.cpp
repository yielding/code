#include <iostream>
#include <vector>
#include <algorithm>

#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

int main(int argc, char* argv[])
{
  auto v = vector { 8, 7, 3 };

  auto r0 = v | views::filter([](int x) { return x > 4; });
  auto s0 = std::accumulate(r0.begin(), r0.end(), 0);

  cout << s0 << endl;

  // NOTICE
  // common_range provides ranges::begin, ranges::end for the old STL algorithm
  //
  auto r1 = v | views::take_while([](int x) { return x > 4; })
              | views::common;

  auto s1 = std::accumulate(r1.begin(), r1.end(), 0);
  cout << s1 << endl;

  return 0;
}
