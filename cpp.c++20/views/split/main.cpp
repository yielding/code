#include <iostream>
#include <vector>
#include <string>
#include <range/v3/view/split.hpp>
#include <range/v3/core.hpp>

using namespace ranges::views;
using namespace ranges;
using namespace std;

using strings = vector<string>;

int main(int argc, char *argv[])
{
  auto s = "hello world"s;
  auto r = s | split(' ') | to<strings>;

  cout << all(r);
  
  return 0;
}
