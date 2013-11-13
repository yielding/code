#include <bitset>
#include <iostream>
#include <boost/dynamic_bitset.hpp>

using namespace std;

template <int N> 
uint8_t reverse_bits(uint8_t n)
{
  bitset<N> b(n);

  auto s = b.to_string();
  reverse(s.begin(), s.end());

  return bitset<N>(s).to_ulong();
}

int main(int argc, const char *argv[])
{
  auto r = reverse_bits<8>(3);
  cout << (int)r << endl;

  // considering move constructor
  // below expression seems to be okay
  auto ds = boost::dynamic_bitset<>(64, 0xf0);
  cout << ds;

  return 0;
}
