#include <iostream>

using namespace std;

constexpr unsigned factorial(unsigned x)
{
  return x == 0 ? 1 : x * factorial(x - 1);
}

int main(int argc, const char *argv[])
{
  int a[factorial(6)];
  
  return 0;
}
