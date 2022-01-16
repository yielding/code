#include <iostream>
#include <string>
#include <sstream>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/take.hpp>
#include <range/v3/view/istream.hpp>

using namespace std;
      namespace view = ranges::views;

int main(int argc, char *argv[])
{
  auto w = string{"a b c"};
  auto s = stringstream{w};
  auto r = ranges::istream<char>(s) | view::take(2);

  cout << view::all(r);

  return 0;
}
