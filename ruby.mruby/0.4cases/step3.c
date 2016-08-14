/*
 [step3] call ruby method from C.
*/

#include "mruby.h"
#include "mruby/proc.h"
#include <stdio.h>

void _error(const char* s){
  printf("ERROR: %s\n", s);
  exit(1);
}

void plus(mrb_state *mrb, mrb_value a, mrb_value b)
{
  mrb_value v;

  v = mrb_funcall(mrb, mrb_top_self(mrb), "plus", 2, a, b);

  printf("mrb_type(v)=%d\n", mrb_type(v));
  mrb_funcall(mrb, mrb_top_self(mrb), "p", 1, v);
}

int main()
{
  mrb_state *mrb;
  int n;
  FILE* f;
  mrb_value str;
  mrb_value z;

  mrb = mrb_open();
  f = fopen("step3.rb", "r");
  if(f==NULL){ _error("file not found."); }
  n = mrb_load_file(mrb, f);
  fclose(f);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_nil_value());

  mrb_funcall(mrb, mrb_top_self(mrb), "hello3", 0);

  str = mrb_str_new_cstr(mrb, "string from c-string");
  mrb_funcall(mrb, mrb_top_self(mrb), "puts", 1, str);
  mrb_funcall(mrb, mrb_top_self(mrb), "p", 1, str);

  z = mrb_funcall(mrb, mrb_top_self(mrb), "tak", 3,
    mrb_fixnum_value(24),
    mrb_fixnum_value(16),
    mrb_fixnum_value( 8)
  );

  str = mrb_str_new_cstr(mrb, "z=");
  mrb_funcall(mrb, mrb_top_self(mrb), "print", 2, str, z);
  mrb_funcall(mrb, mrb_top_self(mrb), "puts", 0);

  if(mrb_type(z) == MRB_TT_FIXNUM){
    int int_z = mrb_fixnum(z);
    printf("z=%d\n", int_z);
  }

  plus(mrb, mrb_fixnum_value(1),        mrb_fixnum_value(1)       );
  plus(mrb, mrb_str_new_cstr(mrb, "1"), mrb_str_new_cstr(mrb, "1"));

  mrb_close(mrb);
}

/*
% gcc -o step3 step3.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a -lm
% ./step3
welcome to step3.
hello3 called.
string from c-string
"string from c-string"
z=9
z=9
mrb_type(v)=3
2
mrb_type(v)=15
"11"
*/
