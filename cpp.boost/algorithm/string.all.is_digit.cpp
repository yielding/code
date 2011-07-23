#include <iostream>
#include <boost/algorithm/string.hpp>

using namespace std;
using namespace boost;

int main(int argc, char *argv[])
{
  cout << all("12345678", is_digit());
  cout << all("1234a678", is_digit());
  // result
  // 10

  return 0;
}
