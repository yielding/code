#include <iostream>
#include <cstdint>
#include <cassert>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto const s = string{"CamelCaseIsGone"};
  auto result
    = s 
    | rv::chunk_by([](uint8_t, uint8_t b) { return islower(b); })
    | rv::join('_')
    | rv::transform([](char c) { return tolower(c); })
    | rg::to<string>
    ;

  assert(result == "camel_case_is_gone");

  return 0;
}