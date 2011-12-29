// Substition Failure Is Not An Error
#include <iostream>

using namespace std;

struct Test
{
  typedef int type;
};

template <typename T>         // 1
void f(typename T::type)
{}

template <typename T>         // 2
void f(T)
{}

int main(int argc, char const* argv[])
{
  f<Test>(10);  // called #1
  f<int>(10);   // called #2

  return 0;
}
