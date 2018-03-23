#include <iostream>

using namespace std;

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

int main(int argc, const char *argv[])
{
  cout << sum(1, 2, 3, 4, 5) << endl;
  cout << sum(3.14, 2.178, 2.23606) << endl;
  cout << avg(1, 2, 3);
  
  return 0;
}
