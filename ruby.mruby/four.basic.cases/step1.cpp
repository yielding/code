//
// [step1] run ruby code.
//
#include <mruby.h>
#include <mruby/compile.h>

auto main() -> int
{
  char code[] = "puts 'hello'; puts 1+2; p({:a=>[1,2,3], :b=>{'c'=>{'d'=>['e', {'f'=>nil}]}}})";

  auto mrb = mrb_open();
  mrb_load_string(mrb, code);
  mrb_close(mrb);

  return 0;
}
