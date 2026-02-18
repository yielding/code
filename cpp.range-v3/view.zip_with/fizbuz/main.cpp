#include <iostream>
#include <array>
#include <algorithm>
#include <range/v3/all.hpp>

namespace v = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  using v::cycle, v::iota, v::take, v::transform, v::zip_with;

  array<string, 3> fiz{"", "", "fiz"};
  array<string, 5> buz{"", "", "", "", "buz"};

  auto fizzes = fiz | cycle;
  auto buzzes = buz | cycle;

  auto fiz_buz = zip_with(plus{}, fizzes, buzzes);

  auto ints = iota(1) 
            | transform([](int x) { return to_string(x); });

  auto rng0 = zip_with([](auto a, auto b) { return max(a, b); }, 
              fiz_buz, ints);

  cout << v::all(rng0 | take(20));

  return 0;
}