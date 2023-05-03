#include <iostream>

using namespace std;

template <class... Args>
size_t f()
{
  return sizeof...(Args);
}

int main()
{
  cout << f<>() << '\n'
       << f<int>() << '\n'
       << f<char, int, double>() << '\n';
}
