#include <string>
#include <iostream>
#include <fstream>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/data.h>
#include <mruby/class.h>
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
  CDJukebox(int id) 
  { 
    unit_id = id;
    cout << "c: CDJukebox constructed with id: " << id << "\n"; 
  }

  ~CDJukebox() 
  { 
    cout << "c: CDJukebox destructed\n";  
    cout.flush();
  }

  void seek(int disk, int track)
  {
    cout << "c: disk: " << disk << ", track: " << track << endl;
  }

  double avg_seek_time()
  {
    return 1.23;
  }

  /* 
  // TODO
  void progress(int percent)
  {
    if (mrb_block_given_p())
    {
      if (percent > 100) percent = 100;
      if (percent <   0) percent = 0;
      mrb_yield(mrb_fixnum_value(percent));
    }
  }
  */

  void uint(int id) { unit_id = id;   }

  int unit()        { return unit_id; }

  int   status;
  int   request;
  void* data;
  char  pending;
  int   unit_id;
  void* stats;
};

////////////////////////////////////////////////////////////////////////////////
//
// Wrapping cpp class to Ruby
//
////////////////////////////////////////////////////////////////////////////////

void jukebox_deleter(mrb_state* mrb, void* p)
{
  cout << "c: jukebox_deleter is called\n";

  auto jukebox = (CDJukebox*)p;
  delete jukebox;
}

struct mrb_data_type jukebox_type = {
  "CDJukebox", jukebox_deleter
};

////////////////////////////////////////////////////////////////////////////////
//
// ruby wrapper for CDJukebox
// REMARK: time.c, array.c 및 기타 mruby/src source 참고
//
// MRI ruby 예제의 cd_alloc과 cd_initialize를 합치다.
//
////////////////////////////////////////////////////////////////////////////////
mrb_value cd_initialize(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  if (jb)
    jukebox_deleter(mrb, jb);

  // TODO
  if (mrb->ci->argc == 0)
  {
  }
  else
  {
    // mrb_get_args(mrb, "oo....", );
    // jb
  }

  mrb_int unit;
  mrb_get_args(mrb, "i", &unit);

  jb = new CDJukebox(unit);

  DATA_PTR(self)  = (void*)jb;
  DATA_TYPE(self) = &jukebox_type;

  return self;
}

mrb_value cd_seek(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);

  mrb_int disk, track;
  mrb_get_args(mrb, "ii", &disk, &track);
  jb->seek(disk, track);

  return self;
}

mrb_value cd_unit(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  if (jb == nullptr)
    cout << "c: jb is null\n";

  int  id = jb->unit();

  cout << "id: " << id << endl;

  return mrb_fixnum_value(id);
}

void init_jukebox(mrb_state* mrb)
{
  RClass* jb = mrb_define_class(mrb, "CDJukebox", mrb->object_class);
  // 
  // REMARK: array.c, 참고 데이타 
  //
  MRB_SET_INSTANCE_TT(jb, MRB_TT_DATA);

  mrb_define_method(mrb, jb, "initialize", cd_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "seek", cd_seek, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "unit", cd_unit, ARGS_NONE());
}

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  auto load_script = [](char const* script) {
    ifstream ifs(script);
    string line, code;
    while (getline(ifs, line)) 
      code += line + "\n";

    return code;
  };

  auto mrb = mrb_open();
  init_jukebox(mrb);

  auto code = load_script("myscript.rb");
  auto p = mrb_parse_string(mrb, code.c_str());
  auto n = mrb_generate_code(mrb, p->tree);
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  if (mrb->exc)
    mrb_p(mrb, mrb_obj_value(mrb->exc));

  mrb_close(mrb);

  return 0;
}
