#include <iostream>
#include <cassert>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto shift = 3;

  auto const s = string{"go ahead"};
  auto letters 
    = s
    | rv::filter([](char c) { return isalpha(c); });

  auto shift_p = (shift < 0) ? (shift + 26) : shift;

  auto alphas 
    = rv::closed_iota('a', 'z') 
    | rv::cycle
    | rv::drop(shift_p);

  auto get_letter = [alphas](char c) {
    return alphas | rv::drop(c - 'a') | rv::take(1);
  };

  auto encoded = letters 
    | rv::for_each(get_letter)
    | rg::to<string>;

  assert(encoded == "jrdkhdg");

  return 0;
}