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
    cout << "CDJukebox constructed with id: " << id << "\n"; 
  }

  ~CDJukebox() 
  { 
    cout << "CDJukebox destructed\n";  
    cout.flush();
  }

  void seek(int disk, int track)
  {
    cout << "disk: " << disk << ", track: " << track << endl;
  }

  double avg_seek_time()
  {
  }

  void uint_id(int id) { unit_id = id;   }

  int unit()           { return unit_id; }

  int   status;
  int   request;
  void* data;
  char  pending;
  int   unit_id;
  void* stats;
};
 
static struct RClass* CDJukeboxClass;

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
// 원 예제의 cd_alloc과 cd_initialize를 합치다.
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
    cout << "jb is null\n";

  int  id = jb->unit();

  cout << "id: " << id << endl;

  return mrb_fixnum_value(id);
}

void init_jukebox(mrb_state* mrb)
{
  CDJukeboxClass = mrb_define_class(mrb, "CDJukebox", mrb->object_class);
  // 
  // REMARK: array.c, time.c 참고 데이타 타입을 줘야한다. 아니면 cd_unit등을 호출할때 
  // exception
  //
  MRB_SET_INSTANCE_TT(CDJukeboxClass, MRB_TT_DATA);

  mrb_define_method(mrb, CDJukeboxClass, "initialize", cd_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, CDJukeboxClass, "seek", cd_seek, ARGS_REQ(1));
  mrb_define_method(mrb, CDJukeboxClass, "unit", cd_unit, ARGS_NONE());
}

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char *argv[])
{
  auto load_script = [](string script) {
    ifstream ifs(script.c_str());
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
