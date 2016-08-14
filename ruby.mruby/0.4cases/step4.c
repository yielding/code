/*
 [step4] call C function from Ruby.
*/

#include "mruby.h"
#include "mruby/proc.h"
#include "mruby/string.h"
#include <stdio.h>
#include <string.h>

void _error(const char* s){
  printf("ERROR: %s\n", s);
  exit(1);
}

mrb_value plus(mrb_state *mrb, mrb_value self)
{
  mrb_value arg1;
  mrb_value arg2;
  mrb_value ret;

  mrb_get_args(mrb, "oo", &arg1, &arg2);
  // ret = mrb_funcall(mrb, arg1, "+", 1, arg2);

  if(mrb_type(arg1)==MRB_TT_FIXNUM && mrb_type(arg2)==MRB_TT_FIXNUM){
    return mrb_fixnum_value(mrb_fixnum(arg1) + mrb_fixnum(arg2));
  }

  if(mrb_type(arg1)==MRB_TT_STRING && mrb_type(arg2)==MRB_TT_STRING){
    // printf("arg1=%s\n", mrb_string_value_cstr(mrb, &arg1));
    // printf("arg2=%s\n", mrb_string_value_cstr(mrb, &arg2));
    char* s1    = mrb_string_value_cstr(mrb, &arg1);
    char* s2    = mrb_string_value_cstr(mrb, &arg2);
    char* s_ret = malloc(strlen(s1)+strlen(s2)+1);
    s_ret[0] = '\0';
    strcat(s_ret, s1);
    strcat(s_ret, s2);
    ret = mrb_str_new_cstr(mrb, s_ret);
    free(s_ret);
    return ret;
  }

  return mrb_nil_value();
}

int main()
{
  mrb_state *mrb;
  int n;
  FILE* f;
  struct RClass* cObject;

  mrb = mrb_open();
  cObject = mrb_class_obj_get(mrb, "Object");
  mrb_define_method(mrb, cObject, "plus", plus, ARGS_ANY());
  //mrb_define_singleton_method(mrb, mrb_top_self(mrb), "plus", plus, ARGS_REQ(2));
  f = fopen("step4.rb", "r");
  if(f==NULL){ _error("file not found."); }
  n = mrb_load_file(mrb, f);
  fclose(f);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_nil_value());
  mrb_close(mrb);
}

/*
% gcc -o step4 step4.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a -lm
% ./step4
welcome to step4.
plus(1,2) => 3
plus('1','2') => "12"
hello world from mruby
*/
