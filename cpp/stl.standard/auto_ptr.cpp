#include <memory>
#include <iostream>

//
// A simple structure.
//

struct X
{
  X (int i = 0) : m_i(i) { }
  int get() const { return m_i; }
  int m_i;
};

using namespace std;

int main ()
{
  //
  // b will hold a pointer to an X.
  //
  auto_ptr<X> b(new X(12345));
  //
  // a will now be the owner of the underlying pointer.
  //
  auto_ptr<X> a = b;
  //
  // Output the value contained by the underlying pointer.
  //
  cout << a->get() << endl;
  //
  // The pointer will be delete'd when a is destroyed on leaving scope.
  //    
  return 0;
}
