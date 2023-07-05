#include <iostream>
#include <string>
#include <array>
#include <algorithm>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char* argv[])
{
  array<string, 3> fiz{"", "", "fiz"};
  array<string, 5> buz{"", "", "", "", "buz"};

  auto fizzes = fiz | v::cycle;
  auto buzzes = buz | v::cycle;

  auto fiz_buz = v::zip_with(plus{}, fizzes, buzzes);

  auto ints = v::iota(1) 
            | v::transform([](int x) { return to_string(x); });

  auto rng0 = v::zip_with([](auto a, auto b) { return max(a, b); }, 
              fiz_buz, ints);

  cout << v::all(rng0 | v::take(20));

  return 0;
}