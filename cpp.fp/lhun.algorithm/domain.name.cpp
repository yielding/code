#include <iostream>
#include <vector>
#include <string>
#include <format>
#include <cassert>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

auto get_domain_from(string const& s) -> string
{
  using v::reverse, v::take_while, g::to;

  auto rng = s 
    | reverse
    | take_while ([](char ch) { return ch != '.'; });

  return rng | reverse | to<string>();
}

void pr(string const& s)
{
  cout << s << endl;
}

void extention_retrieval()
{
  auto const& d0 = string("https://en.test.og");
  auto const& d1 = string("yielding@gmdsoft.com");

  pr(get_domain_from(d0));
  pr(get_domain_from(d1));
}

void power(int base, int exp)
{
  auto rng = v::repeat_n(base, exp);

  cout << g::accumulate(rng, 0) << endl;
  cout << g::accumulate(rng, 1, multiplies{}) << endl;
}

string luhn_algorithm(vector<int> card_nums)
{
  using v::stride, v::concat, v::drop, v::transform;

  auto es = card_nums | stride(2);
  auto os = card_nums | drop(1) | stride(2);
  auto trans_es = es  | transform([](int x) {
      auto y = x * 2;
      return y < 9 ? y : y - 9; });

  auto concated = concat(os, trans_es);
  auto sum = g::accumulate(concated, 0);

  return sum % 10 == 0 ? "ok" : "wrong";
}

int main(int agc, char* agv[])
{
  auto no = vector{4,5,3,9,1,4,8,8,0,3,4,3,6,4,6,7};
  assert(luhn_algorithm(no) == "ok");

  return 0;
}