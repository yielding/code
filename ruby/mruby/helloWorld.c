#include <stdlib.h>
#include <stdio.h>

/* Include the mruby header */
#include <mruby.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/data.h>
// to be replaced by #include <mruby/compile.h>
#include <compile.h>

static mrb_sym id_push;

mrb_value 
t_init(mrb_state* mrb, mrb_value self)
{
  mrb_value arr;
  arr = mrb_ary_new(mrb);
  mrb_iv_set(mrb, self, mrb_intern(mrb, "@arr"), arr);
  return self;
}

mrb_value
t_add(mrb_state* mrb, mrb_value self, mrb_value obj)
{
  mrb_value arr;
  arr = mrb_iv_get(mrb, self, mrb_intern(mrb, "@arr"));
  mrb_funcall(mrb, arr, "push", 1, obj);
  return arr;
}

static struct RClass* cTest;
void init_by_test(mrb_state* mrb)
{
  cTest = mrb_define_class(mrb, "MyTest", mrb->object_class);
  mrb_define_method(mrb, cTest, "initialize", t_init, ARGS_NONE());
  mrb_define_method(mrb, cTest, "add", t_add, ARGS_REQ(1));
  id_push = mrb_intern(mrb, "push");
}

int main(void)
{
  struct mrb_parser_state *p;
  mrb_state *mrb = mrb_open();
  //char code[] = "p 'hello world!'";
  char code[] = "t = MyTest.new; t.add(1); p t.methods";

  printf("Executing Ruby code from C!\n");

  init_by_test(mrb);

  p = mrb_parse_string(mrb, code);
  int n;
  n = mrb_generate_code(mrb, p->tree);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  if (mrb->exc)
    mrb_p(mrb, mrb_obj_value(mrb->exc));

  return 0;
}
