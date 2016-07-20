#include "mruby.h"
#include "mruby/irep.h"

#include <iostream>

using namespace std;

auto we_connected(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  // we need an int from the args
  mrb_int i = 0;

  // retrieve one arg of type int (see mruby.h)
  mrb_get_args(mrb, "i", &i);

  cout << "connect args: " << i << endl;

  // we always need to return a mrb_value object, 
  // and mrb_bool_value converts a C bool to a mrb_value.
  return mrb_bool_value(i >= 2);
}

int main()
{
  auto mrb = mrb_open();
  if (!mrb) { /* handle error */ }

  auto fp = fopen("wiki-example.rb","r");

  // Load the data from the .rb file into the Ruby environment
  mrb_value obj = mrb_load_file(mrb,fp);

  // close the file
  fclose(fp); 

  // First access the module
  auto mod = mrb_module_get(mrb, "WikiExample");

  // Get the cls that is defined in the WikiExample module
  auto cls = mrb_class_get_under(mrb, mod, "WikiManager");

  // Create a new instance of WikiManager, no arguments are needed (0, NULL)
  auto c = mrb_obj_new(mrb, cls, 0, NULL);

  // Call the get_version method on the instance.
  auto res = mrb_funcall(mrb, c, "get_version", 0);

  // Convert the result (a fixed number wrapped in a mrb_value)
  printf("result: %i\n", mrb_fixnum(res));

  // 2
  // add the method to the WikiManager class
  mrb_define_method(mrb, cls, "_we_connected", we_connected, MRB_ARGS_REQ(1));

  // call the connect method on WikiManager
  auto res2 = mrb_funcall(mrb, c, "connect", 0);

  // If crashed, provide exception info
  if (mrb->exc) 
    mrb_print_error(mrb);

  // Close the Ruby environment
  mrb_close(mrb);

  return 0;
}
