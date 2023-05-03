#include <iostream>

using namespace std;

namespace X
{
  template <typename T> void f(T);
}

namespace N
{
  using namespace X;

  enum E { e1 };
  void f(E) 
  {
    cout << "N::f(N::E) called\n";
  }
}

void f(int)
{
  cout << "::f(int) called\n";
}

int main(int argc, char const* argv[])
{
  ::f(N::e1);
  f(N::e1);
  
  return 0;
}
