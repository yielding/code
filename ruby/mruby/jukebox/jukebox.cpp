#include <string>
#include <iostream>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/compile.h>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// CDJukebox
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  
//mrb_value cd_alloc(mrb_state* mrb, mrb_value klass)
//{
  //CDJukebox* jukebox;
//}

mrb_value cd_initialize(mrb_state* mrb, mrb_value self, mrb_value unit)
{
  return self;
}

static struct RClass* cCDJukebox;
void init_jukebox(mrb_state* mrb)
{
  cCDJukebox = mrb_define_class(mrb, "CDJukebox", mrb->object_class);

  mrb_define_method(mrb, cCDJukebox, "initialize", cd_initialize, ARGS_REQ(1));
}


}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  string code = "p 1+1";

  auto mrb = mrb_open();

  init_jukebox(mrb);

  auto p = mrb_parse_string(mrb, code.c_str());
  auto n = mrb_generate_code(mrb, p->tree);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  if (mrb->exc)
    mrb_p(mrb, mrb_obj_value(mrb->exc));

  mrb_close(mrb);

  return 0;
}
