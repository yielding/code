#include <string>
#include <iostream>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/data.h>
#include <mruby/proc.h>
#include <mruby/compile.h>

using namespace std;

namespace {
////////////////////////////////////////////////////////////////////////////////
//
// native CDJukebox
//
////////////////////////////////////////////////////////////////////////////////
struct CDJukebox 
{
  CDJukebox()  { cout << "CDJukebox constructed\n"; }
  ~CDJukebox() { cout << "CDJukebox destructed\n";  }

  void seek(int disk, int track)
  {
  }

  double avg_seek_time()
  {
  }

  void uint_id(int id)
  {
    unit_id = id;
  }

  int   status;
  int   request;
  void* data;
  char  pending;
  int   unit_id;
  void* stats;
};
 
static struct RClass* cCDJukebox;

void jukebox_deleter(mrb_state* mrb, void* p)
{
  auto jukebox = (CDJukebox*)p;
  delete jukebox;

  cout << "jukebox_deleter is called\n";
}

struct mrb_data_type jukebox_type = {
  "CDJukebox", jukebox_deleter
};

////////////////////////////////////////////////////////////////////////////////
//
// ruby wrapper for CDJukebox
// TODO: time.c 참고
//
////////////////////////////////////////////////////////////////////////////////
mrb_value cd_initialize(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  if (jb)
    jukebox_deleter(mrb, jb);

  if (mrb->ci->argc == 0)
  {
  }
  else
  {
    // mrb_get_args(mrb, "oo....", );
    // jb
  }

  jb = new CDJukebox;

  DATA_PTR(self)  = (void*)jb;
  DATA_TYPE(self) = &jukebox_type;

  return self;
}

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
