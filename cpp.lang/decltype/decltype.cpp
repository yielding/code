#include <iostream>

struct A
{
  double x;
};

const A* a = new A();

decltype(a->x) x3;
decltype((a->x)) x4 = x3;

template <typename T, typename U> 
auto add(T t, U u) -> decltype(t + u);

int main(int argc, const char *argv[])
{
  auto f = [](int a, int b) { return a * b; };
  decltype(f) f2{f};

  int i = f(2, 2);
  int j = f2(3, 3);

  std::cout << i << " " << j << std::endl;
  
  return 0;
}
