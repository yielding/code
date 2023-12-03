#include <iostream>

constexpr unsigned factorial(unsigned x)
{
  return x == 0 ? 1 : x * factorial(x - 1);
}

int main(int argc, const char *argv[])
{
  enum { o = "Hello"[4] };

  int a[factorial(6)];
  
  return 0;
}
