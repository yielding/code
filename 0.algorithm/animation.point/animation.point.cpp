#include "point.hpp"

#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;
using namespace std;

using Points = vector<Point>;

constexpr double const_pi() 
{
  return atan(1)*4; 
}

auto get_animation_points(const Points& points, int cut) -> vector<pair<double,int>> 
{
  auto get_theta = [](auto&& p) {
    auto r0 = p[0].rel_to(p[1]);
    auto r1 = p[2].rel_to(p[1]);

    return acos(r1.inner(r0) / (r0.dist() * r1.dist())) * 180.0 / const_pi();
  };

  auto thetas = points | v::sliding(3) | v::transform(get_theta);

  return v::zip(thetas, v::ints)
       | v::filter([cut](auto&& p) { return p.first < cut; })
       | g::to<vector> ;
}

int main(int agc, char* argv[])
{
  Points ps{ {0, 0}, {2, 2}, {4, 4}, {4, 9}, {7, 9} };

  auto res = get_animation_points(ps, 100);

  for (auto&r : res) 
    cout << format("{}: {}\n", r.second, r.first);

  return 0;
}