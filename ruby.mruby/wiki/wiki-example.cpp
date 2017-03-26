#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/value.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/range.h"

#include <iostream>

using namespace std;

// 1. retrieve int arg of type int (see mruby.h)
auto we_connected_with_int(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  // we need an int from the args
  mrb_int i = 0;
  mrb_get_args(mrb, "i", &i);

  return mrb_bool_value(i >= 2);
}

auto we_connected_with_ruby_str(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  // we need an int from the args
  mrb_value result;
  mrb_get_args(mrb, "S", &result);

  cout << "result_with_ruby_str: \n";
  cout << RSTRING_PTR(result);
  cout << endl;

  return mrb_bool_value(true);
}

auto we_connected_with_c_str(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  char* result;
  mrb_get_args(mrb, "z", &result);

  cout << "result_with_c_str: \n";
  cout << string(result);
  cout << endl;

  return mrb_bool_value(true);
}

auto we_connected_with_c_str_len(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  char* result;
  int len;
  mrb_get_args(mrb, "s", &result, &len);

  cout << "result_with_c_str_len: \n";
  cout << string(result, len);
  cout << endl;

  return mrb_bool_value(true);
}

// NOTE: 
//   1. 이 예제에서는 내가 element의 타입이 스트링이라는 것을 알고 있다는 가정이 있다.
auto we_connected_with_ruby_str_arr(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  mrb_value value;
  mrb_int len;
  mrb_get_args(mrb, "A!", &value);

  cout << "result_with_ruby_str_array: \n";
  for (int i=0; i< RARRAY_LEN(value); i++)
  {
    auto element = RARRAY_PTR(value)[i];
    if (!mrb_nil_p(element))
      cout << i << " th: " << RSTRING_PTR(element)<< endl;
  }

  return mrb_bool_value(true);
}

auto load_file(mrb_state* mrb, char const* path) -> bool
{
  auto fp = fopen(path,"r");
  if (!fp)
    return false;

  auto obj = mrb_load_file(mrb, fp);

  fclose(fp); 

  return mrb_fixnum(obj) != 0;
}

int main()
{
  auto mrb = mrb_open();
  if (!mrb) { /* handle error */ }

  if (!load_file(mrb, "wiki-example.rb"))
    return 1;

  // First access the module
  auto mod = mrb_module_get(mrb, "WikiExample");

  // Get the cls that is defined in the WikiExample module
  auto cls = mrb_class_get_under(mrb, mod, "WikiManager");

  // Create a new instance of WikiManager, no arguments are needed (0, NULL)
  auto c0 = mrb_obj_new(mrb, cls, 0, NULL);

  // Call the get_version method on the instance.
  auto r0 = mrb_funcall(mrb, c0, "get_version", 0);

  // Convert the result (a fixed number wrapped in a mrb_value)
  printf("result: %i\n", mrb_fixnum(r0));

  // 2
  // add the method to the WikiManager class
  mrb_define_method(mrb, cls, "_connected_with_int", 
      we_connected_with_int, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, cls, "_connected_with_ruby_str", 
      we_connected_with_ruby_str, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, cls, "_connected_with_c_str", 
      we_connected_with_c_str, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, cls, "_connected_with_c_str_len", 
      we_connected_with_c_str_len, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, cls, "_connected_with_ruby_str_arr", 
      we_connected_with_ruby_str_arr, MRB_ARGS_REQ(1));

  // call the connect method on WikiManager
  auto r1 = mrb_funcall(mrb, c0, "connect", 0);
  cout << "connect result: " << mrb_bool(r1)
       << endl;

  // call the connect method on WikiManager
  auto c1 = mrb_obj_new(mrb, cls, 0, NULL);
  auto r2 = mrb_funcall(mrb, c0, "execute", 1, c1);
  cout << "execute result: " << mrb_fixnum(r2)
       << endl;

  // If crashed, provide exception info
  if (mrb->exc) 
    mrb_print_error(mrb);

  // Close the Ruby environment
  mrb_close(mrb);

  return 0;
}
