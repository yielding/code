#include <iostream>

using namespace std;

// note: c++14
template <typename T> 
T sum(T n)
{
  return n;
}

template <typename T, typename... Args>
T sum(T n, Args... rest)
{
  return n + sum(rest...);
}

template <typename... Args> 
auto avg(Args... args) -> decltype(sum(args...))
{
  return sum(args...) / (sizeof...(args));
}

// note: c++17
template <typename ...Args>
auto sum2(Args&& ...args) -> decltype((args + ...))
{
  return (args + ...);
}

int main(int argc, const char *argv[])
{
  cout << sum2(1.0, 2.0, 3.1) << endl;
  cout << sum(3.14, 2.178, 2.23606) << endl;
  cout << avg(1, 2, 3);
  
  return 0;
}
