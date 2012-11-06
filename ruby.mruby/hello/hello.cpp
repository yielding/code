#include <stdlib.h>
#include <stdio.h>

/* Include the mruby header */
#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>

static void test_free(mrb_state* mrb, void* p);

struct RClass* TestClass;

static const struct mrb_data_type test_type = {
  "TestClass", test_free
};

static void test_free(mrb_state* mrb, void* p)
{
  int* value = (int*)p;
  free(value);

  printf("test_free is called\n");
}

mrb_value test_init(mrb_state* mrb, mrb_value exec)
{
  printf("Test.initialize is called\n");
  return mrb_nil_value();
}

mrb_value test_run(mrb_state* mrb, mrb_value exec)
{
  for (int i=0; i<10; i++)
    printf("Test is running: %d\n", i);

  int* value = (int*)malloc(sizeof(int));
  *value = 10;
  return mrb_obj_value(Data_Wrap_Struct(mrb, TestClass, &test_type, (void*)value));
}

void init_TestClass(mrb_state* mrb)
{
  TestClass = mrb_define_class(mrb, "Test", mrb->object_class);
  mrb_define_method(mrb, TestClass, "initialize", test_init, ARGS_ANY());
  mrb_define_method(mrb, TestClass, "run", test_run, ARGS_ANY());
}

int main()
{
  mrb_state *mrb = mrb_open();
  init_TestClass(mrb);

  char code[] = "$t = Test.new; res = $t.run; p res";
  printf("Executing Ruby code from C!\n");

  auto c = mrbc_context_new(mrb);
  auto p = mrb_parse_string(mrb, code, c);
  auto n = mrb_generate_code(mrb, p);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  if (mrb->exc)
    mrb_p(mrb, mrb_obj_value(mrb->exc));

  return 0;
}

