#include <iostream>
#include <cassert>

#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::view;

using namespace std;

auto get_domain_from(string const& s) -> string
{
  auto rng = s 
    | v::reverse
    | v::take_while ([](char ch) { return ch != '.'; });

  return rng | v::reverse | g::to<string>;
}

void pr(string const& s)
{
  std::cout << s << std::endl;
}

int main(int argc, char* argv[])
{
  auto d0 = string("https://en.test.org");
  auto d1 = string("yielding@gmdsoft.com");

  assert(get_domain_from(d0) == "org");
  assert(get_domain_from(d1) == "com");

  return 0;
}