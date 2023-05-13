#include <iostream>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;
using namespace std;

int main(int argc, char* argv[])
{
  auto target = 15;
  auto triangular_nos = rv::iota(1) | rv::partial_sum;
  auto repeated_target = rv::repeat(target);
  auto diff = rv::zip_with(std::minus{}, repeated_target, triangular_nos)
    | rv::take_while([](int x) { return x > 0; });

  auto diff_pair = rv::zip(diff, rv::iota(2));

  auto divisibles = diff_pair 
    | rv::filter([](auto const& p) {
        return (p.first % p.second == 0);
      });

  auto rng = divisibles 
    | rv::transform([] (auto const& p) {
        auto k = p.first / p.second;
        return rv::iota(k, k + p.second);
      });

  cout << rv::all(rng);

  return 0;
}