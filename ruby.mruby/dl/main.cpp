//
//  main.cpp
//  mruby.dylib.test
//
//  Created by yielding on 2016. 7. 26..
//  Copyright © 2016년 com.hancomgmd. All rights reserved.
//

#include <stdlib.h>
#include <stdio.h>

/* Include the mruby header */ 
#include <mruby.h> 
#include <mruby/proc.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/compile.h>
#include <mruby/string.h>

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
  
  // int* value = (int*)malloc(sizeof(int));
  static int value = 1;
  return mrb_obj_value(Data_Wrap_Struct(mrb, TestClass, &test_type, (void*)&value));
}

void init_TestClass(mrb_state* mrb)
{
  TestClass = mrb_define_class(mrb, "Test", mrb->object_class);
  MRB_SET_INSTANCE_TT(TestClass, MRB_TT_CLASS);
  mrb_define_method(mrb, TestClass, "initialize", test_init, MRB_ARGS_NONE());
  mrb_define_method(mrb, TestClass, "run", test_run, MRB_ARGS_NONE());
}

int main()
{
  auto mrb = mrb_open();
  init_TestClass(mrb);
  
  // char code[] = "$t = Test.new; res = $t.run; p res";
  char code[] = {"puts 'Hello'; puts 1+2+3"};
    
  printf("Executing Ruby code from C!\n");
  
  auto c = mrbc_context_new(mrb);
  auto p = mrb_parse_string(mrb, code, c);
  auto n = mrb_generate_code(mrb, p);
  
  // mrb_run(mrb, n, mrb_top_self(mrb));
  unsigned stack_keep = 0;
  auto result = mrb_vm_run(mrb,
                           n,
                           mrb_top_self(mrb),
                           stack_keep);
  
  if (mrb->exc) // have exception
  {
    mrb_p(mrb, mrb_obj_value(mrb->exc));
    mrb->exc = 0;
  }
  else
  {
    if (!mrb_respond_to(mrb, result, mrb_intern_lit(mrb, "inspect")))
      result = mrb_any_to_s(mrb, result);
    
    mrb_p(mrb, result);
  }
  
  mrbc_context_free(mrb, c);
  mrb_close(mrb);
  
  return 0;
}
