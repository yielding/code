#include <iostream>
#include <stdint.h>

using namespace std;

uint64_t C(uint64_t n, uint64_t k)
{
  return (k == n || k == 0)
    ? 1
    : C(n-1, k-1) + C(n-1, k);
}

int main(int argc, const char *argv[])
{
  cout << C(40, 20);
  
  return 0;
}
