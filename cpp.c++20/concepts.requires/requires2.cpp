#include <concepts>
#include <iostream>
#include <cmath>

using namespace std;

template <integral T>
bool is_power_of_2(T i)
{
  return i > 0 && (i & (i - 1)) == 0;
}

template <floating_point T>
bool is_power_of_2(T x)
{
  int exponent;
  const T mantissa = frexp(x, &exponent);
  return mantissa == T(0.5);
}

template <typename T>
concept arithmetic = integral<T> || floating_point<T>;


int main(int argc, char *argv[])
{
  cout << is_power_of_2(8) << endl;
  cout << is_power_of_2(0.25) << endl;
  
  return 0;
}
