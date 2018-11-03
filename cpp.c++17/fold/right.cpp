#include <iostream>

template <typename ...Args>
int sum(Args&&... args)
{
  return (args + ... + 0);
}

int main(int argc, char *argv[])
{
  std::cout << sum(1, 2, 3, 4);

  return 0;
}
