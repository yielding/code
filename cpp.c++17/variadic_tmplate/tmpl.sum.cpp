#include <iostream>

template <int ...> 
struct my_sum;

template <> 
struct my_sum<>
{
  static const int value = 0;
};

template <int i, int ...tail>
struct my_sum<i, tail...>
{
  static const int value = i + my_sum<tail...>::value;
};

int main(int argc, const char *argv[])
{
  std::cout << my_sum<1, 2, 3, 4, 5>::value;

  return 0;
}
