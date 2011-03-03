#include "rice/Class.hpp"
#include "rice/String.hpp"

using namespace Rice;

Object test_initialize(Object self)
{
  self.iv_set("@foo", 42);
}

Object test_hello(Object /* self */)
{
  String str("hello, world");
  return str;
}

extern "C"
void Init_hello2()
{
  Class rb_cTest =
      define_class("Test")
        .define_method("initialize", &test_initialize)
        .define_method("hello", &test_hello);
}
