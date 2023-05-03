#include <iostream>
// sized class-specific deallocation functions

using namespace std;

struct X {
  static void operator delete(void* ptr, size_t sz)
  {
    cout << "custom delete for size " << sz << '\n';
    ::operator delete(ptr);
  }
  static void operator delete[](void* ptr, size_t sz)
  {
    cout << "custom delete for size " << sz << '\n';
    ::operator delete(ptr);
  }
};

int main() 
{
  X* p1 = new X;
  delete p1;
  X* p2 = new X[10];
  delete[] p2;

  return 0;
}
