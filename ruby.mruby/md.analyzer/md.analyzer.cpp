#include "mruby_basis.h"
#include "data_store_ext.h"
#include "file_system_ext.h"
#include "file_ext.h"

#include <string>
#include <iostream>
#include <fstream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  auto load_script = [](char const* script) -> string {
    ifstream ifs(script);
    string line, code;
    while (getline(ifs, line)) 
      code += line + "\n";

    return code;
  };

  auto mrb = mrb_open();

  init_data_store(mrb);
  init_file_system(mrb);
  init_file(mrb);

  auto code = load_script("myscript.rb");
  auto p    = mrb_parse_string(mrb, code.c_str(), 0);
  auto proc = mrb_generate_code(mrb, p);
  mrb_vm_run(mrb, proc, mrb_top_self(mrb), 0);
  if (mrb->exc)
  {
    mrb_p(mrb, mrb_obj_value(mrb->exc));
    mrb->exc = 0;
  }

  mrb_close(mrb);

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
