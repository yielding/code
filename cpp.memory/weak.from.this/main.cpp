#include <iostream>
#include <memory>

using namespace std;

class MyClass : public enable_shared_from_this<MyClass> 
{
public:
  void print() 
  {
    weak_ptr<MyClass> wp = weak_from_this();

    if (auto sp = wp.lock())
      cout << "Still alive: " << sp.get() << endl;
    else
      cout << "Object not managed by shared_ptr\n";
  }
};

////////////////////////////////////////////////////////////////////////////////
///
///  NOTICE:
///  This code demonstrates the use of `enable_shared_from_this` and `weak_from_this`.
///  It shows how to safely access the shared pointer from within the class,
///  In case, the object is not managed by a `shared_ptr`, it will 
///  print a message indicating that.
///  But, the raw pointer usage is not recommended as it can lead to memory leaks 
///  or undefined behavior. 
///  weak_from_this() is used to safely access the shared pointer 
///  from within the class.
///
////////////////////////////////////////////////////////////////////////////////
int main() 
{
  shared_ptr<MyClass> obj = make_shared<MyClass>();
  obj->print();  // ✅ 정상 출력

  MyClass* raw = new MyClass();
  raw->print();  // ✅ 예외 없이 "Object not managed" 출력
  delete raw;
  
  return 0;
}
