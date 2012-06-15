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
  // TODO: 실제로는 이렇게 코딩하지 않는다. c코드에 c에 없는 개념을
  // 넣는 것은 model code에 UI code를 섞는 것처럼 바람직 하지 않다.
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

  void unit(int id) { unit_id = id;   }

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
// ruby wrapper for CDJukebox
// REMARK: time.c, array.c 및 기타 mruby/src source 참고
//
// MRI ruby 예제의 jb_alloc과 jb_initialize를 합치다.
//
////////////////////////////////////////////////////////////////////////////////
void jb_free(mrb_state* mrb, void* p)
{
  cout << "c: jb_free is called\n";

  auto jukebox = (CDJukebox*)p;
  delete jukebox;
}

struct mrb_data_type jukebox_type = {
  "CDJukebox", jb_free
};

mrb_value jb_initialize(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  if (jb != nullptr)
    jb_free(mrb, jb);

  // REMARK ci : call info
  if (mrb->ci->argc == 0)
  {
    // error, ci->argc should be "1" in this class
  }
  else
  {
    mrb_int unit;
    mrb_get_args(mrb, "i", &unit);

    jb = new CDJukebox(unit);

    DATA_PTR(self)  = (void*)jb;
    DATA_TYPE(self) = &jukebox_type;
  }

  return self;
}

mrb_value jb_seek(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);

  mrb_int disk, track;
  mrb_value blk;

  mrb_get_args(mrb, "ii&", &disk, &track, &blk);
  jb->seek(disk, track);
  for (int p=0; p<100; p+=10)
    mrb_yield(mrb, blk, mrb_fixnum_value(p));

  return self;
}

mrb_value jb_get_unit(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  if (jb == nullptr)
  {
    cout << "c: jb is null\n";
    return mrb_nil_value();
  }

  return mrb_fixnum_value(jb->unit());
}

mrb_value jb_set_unit(mrb_state* mrb, mrb_value self)
{
  mrb_int id;
  mrb_get_args(mrb, "i", &id);
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  jb->unit(int(id));

  return self;
}

mrb_value jb_avg_seek_time(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukebox*)mrb_get_datatype(mrb, self, &jukebox_type);
  return mrb_float_value(jb->avg_seek_time());
}

void init_jukebox(mrb_state* mrb)
{
  // We don't have to pull this jb out
  // RClass* jb
  auto jb = mrb_define_class(mrb, "CDJukebox", mrb->object_class);

  MRB_SET_INSTANCE_TT(jb, MRB_TT_DATA); // REMARK: confer array.c

  mrb_define_method(mrb, jb, "initialize", jb_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "seek", jb_seek, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "avg_seek_time", jb_avg_seek_time, ARGS_NONE());
  mrb_define_method(mrb, jb, "unit",  jb_get_unit, ARGS_NONE());
  mrb_define_method(mrb, jb, "unit=", jb_set_unit, ARGS_REQ(1));
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
