#include <iostream>
#include <cassert>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::view;

using namespace std;

auto get_domain_from(string const& s) -> string
{
  using v::reverse, v::take_while, g::to;

  auto rng = s 
    | reverse
    | take_while ([](char ch) { return ch != '.'; });

  return rng | reverse | to<string>;
}

int main(int argc, char* argv[])
{
  auto d0 = "https://en.test.org"s;
  auto d1 = "yielding@gmdsoft.com"s;

  assert(get_domain_from(d0) == "org");
  assert(get_domain_from(d1) == "com");

  return 0;
}