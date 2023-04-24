#include <iostream>
#include <string>
#include <array>
#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace std;

int main(int argc, char* argv[])
{
  array<string, 3> fiz{"", "", "fiz"};
  array<string, 5> buz{"", "", "", "", "buz"};

  auto fizzes = fiz | view::cycle;
  auto buzzes = buz | view::cycle;

  auto fiz_buz = views::zip_with(std::plus{},
      fizzes, buzzes);

  auto ints = views::iota(1) 
            | views::transform([](int x) { return to_string(x); });

  auto rng = views::zip_with(
      [](auto a, auto b) { return std::max(a, b); }, fiz_buz, ints);

  cout << views::all(rng);

  return 0;
}
