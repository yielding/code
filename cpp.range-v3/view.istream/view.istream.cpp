#include <iostream>
#include <sstream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

int main(int argc, char *argv[])
{
  using v::take, v::all, g::istream;

  auto ss = stringstream{"a b c"};
  auto rs = istream<char>(ss) | take(2);

  cout << all(rs);

  return 0;
}
