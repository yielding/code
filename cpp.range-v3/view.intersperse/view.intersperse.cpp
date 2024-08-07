#include <iostream>
#include <vector>
#include <string>
#include <range/v3/all.hpp>

using namespace ranges::v3;
using namespace view;
using namespace std;

void test_intersperse_string()
{
  auto s0 = "London"s;
  auto s1 = s0 | intersperse('_')
               | to<string>();

  cout << s1 << endl;
}

void test_intersperse_vector()
{
  auto v = { 1, 2, 3, 4 };
  auto r = v | intersperse(0);

  cout << all(r) << endl;
}

int main(int argc, char *argv[])
{
  test_intersperse_string();
  test_intersperse_vector();
  
  return 0;
}
