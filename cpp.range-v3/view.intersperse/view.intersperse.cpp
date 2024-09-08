#include <iostream>
#include <vector>
#include <string>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

void test_intersperse_string()
{
  auto s0 = "London"s;
  auto s1 = s0 | v::intersperse('_')
               | g::to<string>();

  cout << s1 << endl;
}

void test_intersperse_vector()
{
  auto v = { 1, 2, 3, 4 };
  auto r = v | v::intersperse(0);

  cout << v::all(r) << endl;
}

int main(int argc, char *argv[])
{
  test_intersperse_string();
  test_intersperse_vector();
  
  return 0;
}
