#include <string>
#include <fstream>
#include <iostream>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/compile.h>

namespace {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
mrb_value t_init(mrb_state* mrb, mrb_value self)
{
  auto arr = mrb_ary_new(mrb);
  mrb_iv_set(mrb, self, mrb_intern(mrb, "@arr"), arr);
  return self;
}

mrb_value t_add(mrb_state* mrb, mrb_value self)
{
  auto arr = mrb_iv_get(mrb, self, mrb_intern(mrb, "@arr"));
  mrb_value obj;
  mrb_get_args(mrb, "o", &obj);
  mrb_funcall(mrb, arr, "push", 1, obj);

  return arr;
}

static struct RClass* cTest;
void init_by_test(mrb_state* mrb)
{
  cTest = mrb_define_class(mrb, "MyTest", mrb->object_class);

  mrb_define_method(mrb, cTest, "initialize", t_init, ARGS_NONE());
  mrb_define_method(mrb, cTest, "add", t_add, ARGS_REQ(1));
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
} 

int main()
{
  using namespace std;

  cout << "Executing Ruby code from C++!\n";

  ifstream ifs("mytest.rb");
  string line, code;
  while (getline(ifs, line)) 
    code += line + "\n";

  cout << code;

  auto mrb = mrb_open();

  init_by_test(mrb);

  auto c = mrbc_context_new(mrb);
  auto p = mrb_parse_string(mrb, code.c_str(), c);
  auto n = mrb_generate_code(mrb, p);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  if (mrb->exc) // exception?
    mrb_p(mrb, mrb_obj_value(mrb->exc));

  mrb_close(mrb);

  return 0;
}
