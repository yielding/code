#include "rice/Data_Type.hpp"
#include "rice/Constructor.hpp"

#include <string>

using namespace Rice;

class Test
{
public:
  // char const* hello()
  std::string hello()
  {
    return "hello world";
  }
};

extern "C"
void Init_tt()
{
  RUBY_TRY
  {
    define_class<Test>("Test")
      .define_constructor(Constructor<Test>())
      .define_method("hello", &Test::hello);
  }
  RUBY_CATCH
}
