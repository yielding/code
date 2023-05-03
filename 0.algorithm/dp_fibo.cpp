#include <vector>
#include <iostream>
#include <stdint.h>

using namespace std;

uint32_t fibo(uint32_t n)
{
  if (n == 0 || n == 1)
    return 1;

  vector<uint64_t> cache(n+1, 0);
  cache[0] = 0;
  cache[1] = 1;

  for (uint32_t i=2; i<=n; i++)
    cache[i] = cache[i-1] + cache[i-2];

  return cache[n];
}

int main(int argc, char const *argv[])
{
  for (int i=1; i<1000; i++)
    cout << fibo(i) << endl;

  return 0;
}
