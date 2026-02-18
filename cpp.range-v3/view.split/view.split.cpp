#include <iostream>
#include <forward_list>
#include <range/v3/all.hpp>

using namespace ranges::views;
using namespace ranges;
using namespace std;

using strings = forward_list<string>;

auto main(int argc, char *argv[]) -> int
{
  auto s = "hello world"s;
  auto r = s | split(' ') | to<strings>;

  cout << all(r) << endl;

  auto ss = r | join(',') | to<string>;
  cout << ss << endl;
  
  return 0;
}
