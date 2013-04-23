#include <iostream>
#include <memory>

using namespace std;

struct value 
{
  value(const int&) { cout << "l-val ctor" << endl; }
  value(int&&)      { cout << "r-val ctor" << endl; }
};

template <typename T>
shared_ptr<value> make(T&& __v) 
{
  return shared_ptr<value>(new value(forward<T>(__v)));
}

int main()
{
  int a;

  auto v1 = make(a);
  auto v2 = make(3);
}
