#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <cassert>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;

struct point
{
  int x;
  int y;
};

int main(int argc, char* argv[])
{
  using namespace std;

  vector<point> v{ {2, 7}, {6, 5}, {7, 1}, {2, 2} };

  auto product = [](auto&& r) { return r[0].x * r[1].y - r[0].y * r[1].x; };

  auto areas
    = rv::concat(v, v | rv::take(1))  // closed
    | rv::sliding(2)                  // adjacent_vertices
    | rv::transform(product);

  auto area = 0.5 * abs(rg::accumulate(areas, 0));
  assert(area == 19.5);

  return 0;
}