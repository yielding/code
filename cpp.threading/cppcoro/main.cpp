#include <cppcoro/generator.hpp>
#include <iostream>

auto counter(int n) -> cppcoro::generator<int> 
{
  for (int i = 0; i < n; i++)
    co_yield i;
}

int main() 
{
  for (int v : counter(5)) cout << v << " ";

  cout << "\n";

  return 0;
}
