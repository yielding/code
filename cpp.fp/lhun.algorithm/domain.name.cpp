#include <iostream>
#include <vector>
#include <string>
#include <format>
#include <cassert>

#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::view;

using std::string, std::format, std::vector;

auto get_domain_from(string const& s) -> string
{
  auto rng = s 
    | rv::reverse
    | rv::take_while ([](char ch) { return ch != '.'; });

  return rng | rv::reverse | rg::to<string>();
}

void pr(string const& s)
{
  std::cout << s << std::endl;
}

void extention_retrieval()
{
  auto const& d0 = string("https://en.test.org");
  auto const& d1 = string("yielding@gmdsoft.com");

  pr(get_domain_from(d0));
  pr(get_domain_from(d1));
}

void power(int base, int exp)
{
  auto rng = rv::repeat_n(base, exp);
  std::cout << rg::accumulate(rng, 0) << std::endl;
  std::cout << rg::accumulate(rng, 1, std::multiplies{}) << std::endl;
}

string luhn_algorithm(vector<int> card_nums)
{
  auto es = card_nums | rv::stride(2);
  auto os = card_nums | rv::drop(1) | rv::stride(2);
  auto trans_es = es  | rv::transform([](int x) {
      auto y = x * 2;
      return y < 9 ? y : y - 9; });

  auto concated = rv::concat(os, trans_es);
  auto sum = rg::accumulate(concated, 0);

  return sum % 10 == 0 ? "ok" : "wrong";
}

int main(int argc, char* argv[])
{
  auto no = vector{4,5,3,9,1,4,8,8,0,3,4,3,6,4,6,7};
  assert(luhn_algorithm(no) == "ok");

  return 0;
}