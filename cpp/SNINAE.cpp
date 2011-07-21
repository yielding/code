// Substition failure is not an error
#include <iostream>

using namespace std;

struct Test
{
  typedef int type;
};

template <typename T> 
void f(typename T::type)
{}

template <typename T> 
void f(T)
{}

int main(int argc, char const* argv[])
{
  f<Test>(10);  // called #1
  f<int>(10);   // called #2

  return 0;
}

