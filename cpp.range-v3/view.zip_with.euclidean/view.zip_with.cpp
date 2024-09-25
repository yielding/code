#include <iostream>
#include <cmath>
#include <cassert>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  using v::zip_with, v::transform, g::accumulate;

  auto v1 = { 0.0, 1.5, 5.0 };
  auto v2 = { 8.2, 2.5, 4.1 };

  auto rpow = zip_with(minus{}, v1, v2) | transform([](double x) { return x*x; });
  auto dist = sqrt(accumulate(rpow, 0.0));
  assert(abs(dist - 8.30963) < 0.00001);
  cout << dist << endl;
  
  return 0;
}