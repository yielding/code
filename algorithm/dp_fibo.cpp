#include <map>
#include <cstdlib>
#include <iostream>

typedef std::map<int, int> result_t;
typedef std::map<int, int>::iterator result_it;

result_t res;

uint32_t fibo(uint32_t n)
{
  if (n == 0 || n == 1)
    return 1;
  
  result_it it = res.find(n);
  if (it != res.end())
    return it->second;

  res[n] = fibo(n-1) + fibo(n-2);
  return res[n];
}

int main(int argc, char const *argv[])
{
  /* code */
  std::cout << fibo(atoi(argv[1]));
  return 0;
}
