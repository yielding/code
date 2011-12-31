#include "ruby19eval.h"

extern "C" void Init_Test();
extern "C" void Init_Test2();
extern "C" void Init_map();

int main()
{
  Ruby19Eval* rb = Ruby19Eval::instance();
  Init_Test();
  Init_Test2();
  Init_map();

  rb->run_file("test.rb");
  rb->delete_instance();
}
