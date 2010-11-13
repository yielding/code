#include <map>
#include <cstdlib>
#include <iostream>
#include <cassert>

typedef std::map<uint64_t, uint64_t> fibo_map;

fibo_map fiboes;

uint32_t fibo(uint64_t n)
{
  if (n == 0 || n == 1)
    return 1;
  
  auto it = fiboes.find(n);
  if (it != fiboes.end())
    return it->second;

  fiboes[n] = fibo(n-1) + fibo(n-2);

  return fiboes[n];
}

int main(int argc, char const *argv[])
{
  assert(argc == 2);

  std::cout << fibo(atoi(argv[1]));

  return 0;
}
