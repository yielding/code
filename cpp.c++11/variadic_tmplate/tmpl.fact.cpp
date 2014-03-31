#include <iostream>
#include <stdint.h>

template <uint64_t N> 
struct fact { enum { value = N * fact<N-1>::value }; };

template <>
struct fact<0l> { enum { value = 1l }; };

int main(int argc, const char *argv[])
{
  std::cout << fact<5>::value;

  return 0;
}
