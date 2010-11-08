#include <iostream>

using namespace std;

template <unsigned long N>
struct binary 
{
  enum { value = binary<N/10>::value << 1 | (N%10) };
};

template <>
struct binary<0>
{
  enum { value = 0 };
};

int main()
{
  cout << binary<1>::value    << endl;
  cout << binary<10>::value   << endl;
  cout << binary<110>::value  << endl;
  cout << binary<1100>::value << endl;

  return 0;
}
