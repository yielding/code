#include <generator>
#include <ranges>
#include <iostream>

using namespace std;

auto fib() -> generator<int> 
{
  co_yield 0;                 // (1)
  auto a = 0;
  auto b = 1;

  for (auto n : views::iota(0))
  {
    auto next = a + b;
    a = b;
    b = next;
    co_yield next;            // (2)   
  }
}

int main() 
{
  for (auto f : fib() | views::take(10))
    cout << f << " ";

  return 0;
}
