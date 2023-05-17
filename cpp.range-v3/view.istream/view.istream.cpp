#include <iostream>
#include <string>
#include <sstream>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/take.hpp>
#include <range/v3/view/istream.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int argc, char *argv[])
{
  auto ss = stringstream{"a b c"};
  auto rs = g::istream<char>(ss) | v::take(2);

  cout << v::all(rs);

  return 0;
}
