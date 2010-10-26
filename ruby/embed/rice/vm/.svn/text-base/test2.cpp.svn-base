#include "rice/Class.hpp"
#include "rice/Object.hpp"
#include "rice/Data_Type.hpp"
#include "rice/Constructor.hpp"

#include <iostream>

using namespace Rice;
using namespace std;

class Test2
{
public:
  Test2() { cout << "Test2 is constructed\n"; }
  ~Test2() { cout << "Test2 is destructed\n"; }

  string hello()
  {
    return "hello world2 from Test2::hello";
  }
};

extern "C" void Init_Test2()
{
  Data_Type<Test2> rb_cTest2 = 
    define_class<Test2>("Test2")
      .define_constructor(Constructor<Test2>())
      .define_method("hello", &Test2::hello);
}
