#include <iostream>

using namespace std;

inline constexpr long double operator"" _deg(long double deg)
{
  return deg * 3.1415926 / 180;
}


void operator"" _print(const char* str)
{
  cout << str;
}

int main(int argc, const char *argv[])
{
  cout << 90.0_deg;

  0x123_print;

  return 0;
}
