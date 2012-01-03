#include <random>
#include <functional>
#include <iostream>

using namespace std;

void f(int n1, int n2, int n3, int const& n4, int n5)
{
  std::cout << n1 << ' ' << n2 << ' ' << n3 << ' '
    << n4 << ' ' << n5 << '\n';
}

int g(int n1)
{
  return n1;
}

int main(int argc, char const* argv[])
{
  using namespace std::placeholders;

  auto n0 = 7;
  auto f1 = std::bind(f, _2, _1, 42, std::cref(n0), n0);
  n0 = 10;
  f1(1, 2, 1001);

  auto f2 = std::bind(f, _3, std::bind(g, _3), _3, 4, 5);
  f2(10, 11, 12);

  default_random_engine e;
  uniform_int_distribution<> d(0, 10);
  auto rnd = std::bind(d, e);
  for (int n=0; n<10; ++n)
    std::cout << rnd() << ' ';

  std::cout << '\n';

  return 0;
}
