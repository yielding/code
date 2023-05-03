#include <iostream>
#include <string>
#include <array>
#include <algorithm>
//#include <range/v3/all.hpp>
#include <ranges>

using namespace ranges;
//using namespace std;

int main(int argc, char* argv[])
{
  array<string, 3> fiz{"", "", "fiz"};
  array<string, 5> buz{"", "", "", "", "buz"};

  auto fizzes = fiz | view::cycle;
  auto buzzes = buz | view::cycle;

  auto fiz_buz 
    = view::zip_with(std::plus{}, fizzes, buzzes);

  auto ints 
    = view::iota(1) 
    | view::transform([](int x) { return to_string(x); });
  auto rng = views::zip_with(
    [](auto a, auto b) { return std::max(a, b); }, 
    fiz_buz, ints);

  std::cout << view::all(rng | view::take(20));

  return 0;
}