#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cassert>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  auto v1 = vector{ 0.0, 1.5, 5.0 };
  auto v2 = vector{ 8.2, 2.5, 4.1 };

  auto rpow = v::zip_with(minus{}, v1, v2) | v::transform([](double x) { return x * x; });
  auto dist = sqrt(g::accumulate(rpow, 0.0));
  cout << dist << endl;
  assert(abs(dist - 8.30963) < 0.00001);
  
  return 0;
}