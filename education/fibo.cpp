#include <iostream>

using namespace std;

unsigned fib(unsigned n)
{
  return n == 0 ? 0 :
         n == 1 ? 1 :
                  fib(n - 1) + fib(n - 2);
}

int main(int argc, char* argv[])
{
  cout << fib(10);

  return 0;
}