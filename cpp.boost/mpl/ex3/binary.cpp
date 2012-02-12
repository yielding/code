#include <boost/static_assert.hpp>

#include <iostream>
#include <stdint.h>

template <uint32_t N> 
struct binary
{
  static const uint32_t digit = N % 10;
  BOOST_STATIC_ASSERT(digit < 2);
  static const uint32_t value = digit + binary<N/10>::value*2;
};

template <> 
struct binary<0>
{
  static const uint32_t value = 0;
};

using namespace std;

int main(int argc, char const* argv[])
{
  cout << binary<10000>::value;
  return 0;
}
