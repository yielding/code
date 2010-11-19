#include <vector>
#include <iostream>

template <class T> 
struct PTS 
{
  enum { IsPointer = 0, IsPointerToDataMember = 0 };
};

template <class T> 
struct PTS<T*> 
{
  enum {
    IsPointer = 1,
    IsPointerToDataMember = 0
  };
};

template <class T, class U> 
struct PTS<T U::* > 
{
  enum {
    IsPointer = 0,
    IsPointerToDataMember = 1
  };
};

struct C { 
  int a() { return 0; } 
};

int main(int argc, char const* argv[])
{
  using namespace std;

  PTS<int> a;
  PTS<int*> b;
  PTS<int C::*> c;

  cout << a.IsPointer << a.IsPointerToDataMember << endl;
  cout << b.IsPointer << b.IsPointerToDataMember << endl;
  cout << c.IsPointer << c.IsPointerToDataMember << endl;
  
  return 0;
}
