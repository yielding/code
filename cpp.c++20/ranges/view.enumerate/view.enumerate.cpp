#include <iostream>
#include <vector>
#include <string>
#include <range/v3/view/enumerate.hpp>

using namespace ranges::v3::views;
using namespace std;

int main(int argc, char *argv[])
{
  auto v = { "apple"s, "banana"s, "kiwi"s };
  for (auto&& [f, s] : v | enumerate) 
    cout << f + 1 << ", " << s << endl;
  
  return 0;
}