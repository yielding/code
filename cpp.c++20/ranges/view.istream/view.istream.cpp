#include <iostream>
#include <string>
#include <sstream>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/take.hpp>
#include <range/v3/view/istream.hpp>

using namespace std;
using namespace ::ranges::views;

int main(int argc, char *argv[])
{
  auto ss = stringstream{"a b c"s};
  auto rs = ::ranges::istream<char>(ss) | take(2);

  cout << all(rs);

  return 0;
}
