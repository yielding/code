#include <iostream>
#include <vector>
#include <string>
#include <range/v3/core.hpp>
#include <range/v3/view/intersperse.hpp>
#include <range/v3/range/conversion.hpp>

using namespace ranges;

void test_intersperse_string()
{
  using namespace std;

  auto s0 = string{"London"};
  auto s1 = s0 | views::intersperse('_')
               | to<string>();

  cout << s1 << endl;
}

void test_intersperse_vector()
{
  using namespace std;

  auto v = vector<int> { 1, 2, 3, 4 };
  auto r = v | views::intersperse(0);

  cout << views::all(r) << endl;
}

int main(int argc, char *argv[])
{
  test_intersperse_string();
  test_intersperse_vector();
  
  return 0;
}
