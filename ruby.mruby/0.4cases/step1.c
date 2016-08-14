/*
 [step1] run ruby code.
*/

#include "mruby.h"
#include "mruby/proc.h"

int main()
{
  mrb_state *mrb;
  int n;

  mrb = mrb_open();
  n = mrb_load_string(mrb, "puts 'hello'; puts 1+2; p({:a=>[1,2,3], :b=>{'c'=>{'d'=>['e', {'f'=>nil}]}}})");
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_nil_value());
  mrb_close(mrb);
}

/*
% gcc -o step1 step1.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a -lm
% ./step1
hello
3
{:a=>[1, 2, 3], :b=>{"c"=>{"d"=>["e", {"f"=>nil}]}}}
*/
