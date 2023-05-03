#include <iostream>
#include <string>
#include <sstream>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/take.hpp>
#include <range/v3/view/istream.hpp>

using namespace ranges::v3::view;
using namespace ranges::v3;

int main(int argc, char *argv[])
{
  auto ss = std::stringstream{"a b c"};
  auto rs = istream<char>(ss) | take(2);

  std::cout << all(rs);

  return 0;
}
