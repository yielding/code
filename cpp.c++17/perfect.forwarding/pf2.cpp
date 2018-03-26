#include <iostream>
#include <memory>
#include <utility>

using namespace std;

struct A
{
  A(int&& n)
  {
    cout << "rvalue overload n=" << n << endl;
  }

  A(int& n)
  {
    cout << "lvalue overload n=" << n << endl;
  }
};

template <typename T, class U> 
unique_ptr<T> make_unique(U&& u)
{
  return unique_ptr<T>(new T(forward<U>(u)));
}

int main(int argc, const char *argv[])
{
  auto p1 = make_unique<A>(2);
  auto  i = 10;
  auto p2 = make_unique<A>(i);
  
  return 0;
}
