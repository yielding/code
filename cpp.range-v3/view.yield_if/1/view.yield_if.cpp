#include <iostream>
#include <string>
#include <vector>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;
namespace ac = ranges::actions;

using namespace std;

auto contains_char(string const& s, char c)
{
  return s.find(c) != string::npos;
}

auto contains_all_digits(string const& s)
{
  auto r = s;
  r |= ac::sort | ac::unique;

  return rg::distance(r) == 9;
}

int main(int argc, char* argv[])
{
  auto rng = rv::iota(100, 1000)
           | rv::for_each([](auto x) {
               auto x_sq = x * x;
               auto s_x  = to_string(x);
               auto s_sq = to_string(x_sq);
               auto s_num = s_x + s_sq;
               return rg::yield_if(
                   !contains_char(s_num, '0') && contains_all_digits(s_num),
                    make_pair(x, s_sq));
           });

  for (auto const& [e1, e2] :rng)
    cout << "(" << e1 << " : " << e2 << ")" << endl;
  
  return 0;
}