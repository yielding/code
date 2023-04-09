/*
 [step4] call C function from Ruby.
*/

#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <cstdio>
#include <cstring>
#include <string>

using namespace std;

void _error(const char* s)
{
  printf("ERROR: %s\n", s);
  exit(1);
}

auto plus_(mrb_state *mrb, mrb_value self) -> mrb_value 
{
  mrb_value arg1, arg2;
  mrb_get_args(mrb, "oo", &arg1, &arg2);

  auto t1 = mrb_type(arg1);
  auto t2 = mrb_type(arg2);

  if (t1 == MRB_TT_FIXNUM && t2 == MRB_TT_FIXNUM)
    return mrb_fixnum_value(mrb_fixnum(arg1) + mrb_fixnum(arg2));

  if (t1 == MRB_TT_STRING && t2 == MRB_TT_STRING) 
  {
    auto s1 = string(mrb_string_value_cstr(mrb, &arg1));
    auto s2 = string(mrb_string_value_cstr(mrb, &arg2));
    auto s3 = s1 + s2;

    return mrb_str_new_cstr(mrb, s3.c_str());
  }

  return mrb_nil_value();
}

int main()
{
  auto mrb = mrb_open();
  auto cObject = mrb_class_get(mrb, "Object");
  mrb_define_method(mrb, cObject, "plus", plus_, MRB_ARGS_ANY());

  auto f = fopen("step4.rb", "r");
  if (f == NULL) 
    _error("file not found.");

  mrb_load_file(mrb, f);
  fclose(f);
  mrb_close(mrb);

  return 0;
}

/*
% gcc -o step4 step4.c -I ../include -I ../src ../mrblib/mrblib.o ../lib/libmruby.a -lm
% ./step4
welcome to step4.
plus(1,2) => 3
plus('1','2') => "12"
hello world from mruby
*/
