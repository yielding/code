#include <iostream>
#include <vector>
#include <stdint.h>

using namespace std;

uint64_t C(uint64_t n, uint64_t k)
{
  vector<vector<uint64_t>> cache(n+1);
  for (int i=0; i<=n; i++) cache[i].resize(n+1, 0);

  for (int i=0; i<=n; i++)
  {
    for (int j=0; j<=std::min<int>(i, k); j++) {
      if (j == 0 || j == i) 
        cache[i][j] = 1;
      else
        cache[i][j] = cache[i-1][j-1] + cache[i-1][j];
    }
  }

  return cache[n][k];
}

int main(int argc, const char *argv[])
{
  cout << C(40, 20);
  
  return 0;
}
