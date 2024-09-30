#include <iostream>
#include <cassert>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int agc, char* agv[])
{
  auto shift = 3;

  auto const s = string{"go ahead"};
  auto letters 
    = s
    | v::filter([](char c) { return isalpha(c); });

  auto shift_p = (shift < 0) ? (shift + 26) : shift;

  auto alphas 
    = v::closed_iota('a', 'z') 
    | v::cycle
    | v::drop(shift_p);

  auto get_letter = [alphas](char c) {
    return alphas | v::drop(c - 'a') | v::take(1);
  };

  auto encoded = letters 
    | v::for_each(get_letter)
    | g::to<string>;

  assert(encoded == "jrdkhdg");

  return 0;
}