#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto main(int argc, char** argv) -> int
{   
  using v::take, v::ints, v::transform, g::accumulate;

  auto rng = v::generate(
    [p=pair{1, 1}]() mutable -> tuple_element<0, pair<int, int>>::type {
      auto [a0, b0] = p; 
      p = {b0, a0 + b0};
      return a0;
    }
  ); 

  auto fib10 = rng | take(10);
  cout << fib10 << endl;

  auto rng2 = ints(1, 10) | transform([](int i) -> int { return i*i; });

  cout << accumulate(rng2 , 0);

  return 0;
}