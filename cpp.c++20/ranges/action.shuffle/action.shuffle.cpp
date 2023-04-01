#include <algorithm>
#include <array>
#include <iostream>
#include <random>
#include <range/v3/all.hpp>

using namespace std;

void print(const auto& a) 
{
  for (const auto e : a) { cout << e << ' '; }
  cout << "\n";
}

int main()
{
  array a{'A', 'B', 'C', 'D', 'E', 'F'};
  print(a);

  random_device rd;
  mt19937 gen{rd()};

  for (int i{}; i != 3; ++i) 
  {
    ::ranges::action::shuffle(a, gen);
    print(a);
  }

  return 0;
}