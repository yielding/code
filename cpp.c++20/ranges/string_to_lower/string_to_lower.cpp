#include <iostream>
#include <string>
#include <cctype>

#include <range/v3/all.hpp>

using namespace ranges::v3;
using std::string;

auto string_to_lower(string const& s) -> string 
{
  return s | views::transform(tolower)
           | to<string>;
}

auto string_only_alnum(string const& s) -> string
{
  return s | views::filter(isalnum)
           | to<string>;
}

int main(int argc, char* argv[])
{
  using namespace std;
  cout << string_to_lower("HHH") << endl;
  cout << string_only_alnum("H!H2H") << endl;

  return 0;
}