/*
 [step2] run ruby code in file.
*/

#include "mruby.h"
#include "mruby/proc.h"
#include <stdio.h>

void _error(const char* s){
  printf("ERROR: %s\n", s);
  exit(1);
}

int main()
{
  mrb_state *mrb;
  int n;
  FILE* f;

  mrb = mrb_open();
  f = fopen("step2.rb", "r");
  if(f==NULL){ _error("file not found."); }
  n = mrb_load_file(mrb, f);
  fclose(f);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_nil_value());
  mrb_close(mrb);
}

/*
% gcc -o step2 step2.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a -lm
% ./step2
welcome to step2.
fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3
fib(5) = 5
fib(6) = 8
fib(7) = 13
fib(8) = 21
fib(9) = 34
fib(10) = 55
n=0
i=1
i=2
i=3
i=4
i=5
i=6
i=7
i=8
i=9
i=10
*/
