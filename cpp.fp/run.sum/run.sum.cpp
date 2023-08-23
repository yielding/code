#include <iostream>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;
using namespace std;

int main(int agc, char* argv[])
{
  auto taget = 15;
  auto triangular_nos = v::iota(1) | v::partial_sum;
  auto repeated_taget = v::repeat(target);
  auto diff = v::zip_with(std::minus{}, repeated_taget, triangular_nos)
    | v::take_while([](int x) { return x > 0; });

  auto diff_pair = v::zip(diff, v::iota(2));

  auto divisibles = diff_pair 
    | v::filter([](auto const& p) {
        return (p.first % p.second == 0);
      });

  auto rng = divisibles 
    | v::transform([] (auto const& p) {
        auto k = p.first / p.second;
        return v::iota(k, k + p.second);
      });

  cout << v::all(rng);

  return 0;
}