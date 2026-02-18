#include <string>
#include <print>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace a = ranges::actions;
namespace g = ranges;

using namespace std;

auto contains_char(string const& s, char c)
{
  return s.find(c) != string::npos;
}

auto contains_all_digits(string const& s)
{
  auto r = s;
  r |= a::sort | a::unique;

  return g::distance(r) == 9;
}

int main(int agc, char* agv[])
{
  using v::iota, v::for_each, g::yield_if;

  auto rng = iota(100, 1000)
           | for_each([](auto x) {
               auto s_num = to_string(x) + to_string(x * x);
               return yield_if(
                   !contains_char(s_num, '0') && contains_all_digits(s_num),
                   make_pair(x, s_num));
           });

  for (auto const& [e1, e2] : rng)
    println("({} : {})", e1, e2);
  
  return 0;
}