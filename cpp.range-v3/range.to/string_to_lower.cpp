#include <string>
#include <cctype>
#include <print>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

auto string_to_lower(string const& s) -> string 
{
  return s | v::transform(::tolower)
           | g::to<string>;
}

auto string_only_alnum(string const& s) -> string
{
  return s | v::filter(::isalnum)
           | g::to<string>;
}

int main(int argc, char* argv[])
{
  println("{}", string_to_lower("HHH"));
  println("{}", string_only_alnum("H!H2H"));

  return 0;
}