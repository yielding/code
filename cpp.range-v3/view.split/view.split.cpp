#include <iostream>
#include <forward_list>
#include <string>
#include <range/v3/all.hpp>

using namespace ranges::views;
using namespace ranges;
using namespace std;

using strings = forward_list<string>;

int main(int argc, char *argv[])
{
  auto s = "hello world"s;
  auto r = s | split(' ') | to<strings>;

  cout << all(r) << endl;

  auto ss = r | join(',') | to<string>;
  cout << ss << endl;
  
  return 0;
}
