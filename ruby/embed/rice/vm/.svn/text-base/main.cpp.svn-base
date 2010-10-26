#include "rubyeval.h"

extern "C" void Init_Test();
extern "C" void Init_Test2();
extern "C" void Init_map();

int main()
{
  RubyEval* rb = RubyEval::instance();
  Init_Test();
  Init_Test2();
  Init_map();

  rb->run_file("test.rb");
  rb->delete_instance();
}
