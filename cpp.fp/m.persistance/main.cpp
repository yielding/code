#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

// NOTICE
// multiplicative persistance
//
auto int_to_range(int n) -> vector<int>
{
  auto s = to_string(n);

  return s 
    | v::transform([](char c) { return c - '0'; })
    | g::to<vector>;
}

auto product(int n) -> int
{
  return g::accumulate(int_to_range(n), 1, multiplies{});
}

int main(int agc, char* agv[])
{
  using v::generate, v::take_while, g::distance;

  auto rng = generate([n=948]() mutable {
      auto prev = n;
      n = product(prev);
      return prev;
      });

  auto r = rng | take_while([](int n) { return n > 9; }) ;

  assert(distance(r) == 4);

  return 0;
}