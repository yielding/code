#include <iostream>
#include <string>
#include <cassert>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;
using namespace std;

auto add_comma(string const& s, int count) -> string
{
  using g::to;
  using v::reverse, v::chunk, v::join;

  auto r0 = s | reverse | chunk(count) | join(',') | to<string>();

  return r0 | reverse | to<string>;
}

auto add_comma2(string const& s, int count) -> string
{
  auto res = s;
  auto len = (int)res.length();
  auto pos = count;
  while (len - pos > 0)
  {
    res.insert(len - pos, ",");
    pos += count;
  }

  return res;
}

int main(int argc, char* argv[])
{
  // assert(add_comma("1234000"s, 3) == "1,234,000");
  // assert(add_comma("12"s, 3) == "12");

  cout << add_comma2("1234000"s, 4);

  return 0;
}