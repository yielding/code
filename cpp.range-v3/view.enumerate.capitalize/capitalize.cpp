#include <iostream>
#include <string>
#include <cassert>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto capitalize(string const& s) -> string
{
  auto third_upper = [](auto const& p) { 
    return p.first % 3 == 0 ? toupper(p.second) : p.second;
  };

  return s | v::enumerate
           | v::transform(third_upper)
           | g::to<string> ;
}

int main(int argc, char *argv[])
{
  auto r = capitalize("consequnce"sr;

  assert(r == "ConSeqUenCe");
  
  return 0;
}