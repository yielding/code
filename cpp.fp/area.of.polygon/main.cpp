#include <string>
#include <cassert>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

struct point
{
  int x;
  int y;
};

int main(int agc, char* agv[])
{
  using namespace std;
  using v::concat, v::take, v::sliding, v::transform, g::accumulate; 

  vector<point> v{ {2, 7}, {6, 5}, {7, 1}, {2, 2} };

  auto product = [](auto&& r) { return r[0].x * r[1].y - r[0].y * r[1].x; };

  auto areas
    = concat(v, v | take(1))  // closed
    | sliding(2)              // adjacent_vertices
    | transform(product)
    ;

  auto area = 0.5 * abs(accumulate(areas, 0));
  assert(area == 19.5);

  return 0;
}