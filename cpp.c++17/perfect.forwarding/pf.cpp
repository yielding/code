#include <iostream>
#include <memory>

using namespace std;

struct value 
{
  value(const int&) { cout << "l-val ctor" << endl; }
  value(int&&)      { cout << "r-val ctor" << endl; }
  ~value()          { cout << "dtor" << endl; }
};

// most specific way to use perfect forwarding
template <typename T>
shared_ptr<value> factory(T&& arg) 
{
  return shared_ptr<value>(new value(forward<T>(arg)));
}

int main()
{
  int a;

  auto v1 = factory(a);
  auto v2 = factory(3);
}
