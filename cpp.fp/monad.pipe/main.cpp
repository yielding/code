#include "monad.pipe.hpp"

#include <iostream>
#include <string>
#include <expected>

using namespace std;
using namespace monad_pipe;

auto s1(const int x) -> expected<int, string> 
{
  if (x < 0) return unexpected("neg");
  return x + 1;
}

auto s2(const int x) -> expected<double, string> 
{
  if (x == 0) return unexpected("zero");
  return x * 2.5;
}

auto main() -> int 
{
  auto r = s1(41)
    | tap_value([](const auto& v) { cout << "[tap_value] s1: " << v << "\n"; })
    | and_then ([](const auto& v) { return s2(v); })
    | or_else  ([](const auto& e) {
             cerr << "[err] " << e << "\n";
             return expected<double,string>{unexpected(e)};
           })
    | monad_pipe::transform([](const double d) { return d + 0.5; });

  if (r) 
    cout << "result = " << *r << "\n";

  return 0;
}
