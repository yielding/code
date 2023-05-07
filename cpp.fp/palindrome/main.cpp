#include <iostream>
#include <string>
#include <cctype>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;
namespace as = ranges::actions;
using namespace std;

int main(int argc, char* argv[])
{
  auto const& s = "A Man, A Plan, A Canal - Panama!"s;

  auto lowered = s 
    | rv::transform([](auto ch) { return tolower(ch); })
    | rv::filter([](char c) { return isalpha(c); })
    ;
  
  auto reversed = lowered | rv::reverse; 

  const int n = rg::distance(reversed) / 2;
  cout << rg::equal(lowered | rv::take(n), reversed | rv::take(n));

  return 0;
}