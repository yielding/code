#include <vector>
#include <print>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  using v::filter, v::common, v::take_while, g::accumulate;

  auto v = vector { 8, 7, 3 };

  auto r0 = v | filter([](int x) { return x > 4; });
  println("{}", accumulate(r0.begin(), r0.end(), 0));

  // NOTICE
  // common_range provides 
  // ranges::begin, ranges::end for the old STL algorithm
  auto r1 = v | take_while([](int x) { return x > 4; })
              | common;

  println("{}", reduce(r1.begin(), r1.end(), 0));

  return 0;
}
