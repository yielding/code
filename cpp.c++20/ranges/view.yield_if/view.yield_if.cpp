#include <iostream>
#include <string>
#include <vector>
#include <range/v3/all.hpp>

using namespace ranges;

auto contains_char(std::string const& s, char c)
{
  return s.find(c) != std::string::npos;
}

auto contains_all_digits(std::string const& s)
{
  auto r = s;
  r |= actions::sort | actions::unique;

  return distance(r) == 9;
}

int main(int argc, char* argv[])
{
  auto rng = views::iota(100, 1000)
           | views::for_each([](auto x) {
               auto x_sq = x * x;
               auto s_x = std::to_string(x);
               auto s_sq = std::to_string(x_sq);
               auto s_num = s_x + s_sq;
               return yield_if (
                   !contains_char(s_num, '0') && contains_all_digits(s_num),
                    std::make_pair(x, s_sq));
           });

  for (auto const& [e1, e2] :rng)
    std::cout << "(" << e1 << " : " << e2 << ")" << std::endl;
  
  return 0;
}