#include <iostream>
#include <typeinfo>

using namespace std;

template <typename T>
class result
{
public:
  result()
  {
  }

  void id()
  {
    cout << typeid(*this).name() << endl;
  }

  virtual ~result() {}
};

template <typename T>
class success: public result<T>
{
public:
  success()
  {
    cout << typeid(*this).name() << endl;
  }
};

template <typename T>
class fail: public result<T>
{
public:
  fail()
  {
    cout << typeid(*this).name() << endl;
  }
};

int main()
{
  success<int> s;
  s.id();

  fail<int> f;
  f.id();
  
  return 0;
}
