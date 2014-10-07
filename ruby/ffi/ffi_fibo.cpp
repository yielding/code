extern "C" {
#include <stdlib.h>
#include <math.h>

int ffi_pow(int a, int n);
int ffi_factorial(int max);
int ffi_fibonacci(int n);
}

int ffi_pow(int a, int n)
{
  return pow(a, n);
}

int ffi_factorial(int max) 
{
  int i=max, result=1;
  while (i >= 2) 
  {
    result *= i--; 
  }

  return result;
}

class fibonacci
{
public:
  int compute(int n)
  {
    int a = 1, b = 1, c, i;

    if (n == 0) 
      return 0;
    
    for (i = 3; i <= n; i++) 
    {
      c = a + b;
      a = b;
      b = c;
    }

    return b;
  }
    

};

int ffi_fibonacci(int n) 
{
  fibonacci fibo;
  return fibo.compute(n);
}
