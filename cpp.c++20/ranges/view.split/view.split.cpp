#include <iostream>
#include <forward_list>
#include <string>
#include <range/v3/view/split.hpp>
#include <range/v3/core.hpp>

using namespace ranges::v3;
using namespace std;

using strings = forward_list<string>;

int main(int argc, char *argv[])
{
  auto s = "hello world"s;
  auto r = s | split(' ') | to<strings>;

  cout << all(r);
  
  return 0;
}
