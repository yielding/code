#include <iostream>
#include <string>

template <typename F> 
void f(F f)
{
  f(10);
}

int main(int argc, char const* argv[])
{
  int x = 10;
  int y = 20;

  auto r = [&](int a) { 
    x += a; 
    y += a + 10; 
  };

  f(r);

  std::cout << x << " " << y << std::endl; // 20, 40
  
  return 0;
}
