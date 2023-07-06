#include <iostream>
#include <cassert>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::view;

using namespace std;

string luhn_algorithm(vector<int> card_nums)
{
  auto es = card_nums | v::stride(2);
  auto os = card_nums | v::drop(1) | v::stride(2);
  auto trans_es = es  | v::transform([](int x) {
      auto y = x * 2;
      return y < 9 ? y : y - 9; });

  auto concated = v::concat(os, trans_es);
  auto sum = g::accumulate(concated, 0);

  return sum % 10 == 0 ? "ok" : "wrong";
}

int main(int argc, char* argv[])
{
  auto no = vector{4,5,3,9,1,4,8,8,0,3,4,3,6,4,6,7};
  assert(luhn_algorithm(no) == "ok");

  return 0;
}