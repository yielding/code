#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

namespace rv = ranges::views;
namespace rg = ranges;
using namespace std;

// NOTICE
// multiplicative persistance
//
auto int_to_range(int n) -> vector<int>
{
  auto s = to_string(n);

  return s 
    | rv::transform([](char c) { return c - '0'; })
    | rg::to<vector>;
}

auto product(int n) -> int
{
  return rg::accumulate(int_to_range(n), 1, multiplies{});
}

int main(int argc, char* argv[])
{
  auto rng = rv::generate([n=948]() mutable {
      auto prev = n;
      n = product(prev);
      return prev;
      });

  auto r = rng
    | rv::take_while([](int n) { return n > 9; })
    ;

  assert(rg::distance(r) == 4);

  return 0;
}