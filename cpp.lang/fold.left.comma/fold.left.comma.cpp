#include <iostream>

struct A
{
  void do_operation(int x) const 
  {
    std::cout << "do with " << x << std::endl;
  }
};

template <typename T, typename... Ints>
void do_operations_with(const T& t, Ints... nums)
{
  (t.do_operation(nums), ...);
}

int main(int argc, char *argv[])
{
  A a;

  do_operations_with(a, 1, 2, 3, 4, 5);

  return 0;
}
