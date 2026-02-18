#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  using v::split, v::transform, g::distance, g::accumulate;

  auto s = "we have been better days"s;
  auto rng = s
    | split(' ')
    | transform([](auto && s) { return distance(s); }) ;

  auto avg = accumulate(rng, 0) / distance(rng);
  
  cout << v::all(rng) << endl;
  cout << avg << endl;

  return 0;
}