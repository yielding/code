#include <string>
#include <vector>
#include <iostream>
#include <fstream>

#include <boost/format.hpp>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/compile.h>

using namespace std;
using namespace boost;

namespace {
////////////////////////////////////////////////////////////////////////////////
//
// native DVD
//
////////////////////////////////////////////////////////////////////////////////
struct DVD
{
  DVD(string n="") : name(n)
  {}

  string name;
};

////////////////////////////////////////////////////////////////////////////////
//
// native CDJukeBox
//
////////////////////////////////////////////////////////////////////////////////
struct CDJukeBox 
{
  CDJukeBox(int id) 
  { 
    unit_id = id;
    cout << "c: CDJukeBox constructed with id: " << id << "\n"; 
  }

  ~CDJukeBox() 
  { 
    cout << "c: CDJukeBox destructed\n";  
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

  void unit(int id) { unit_id = id;   }

  int unit()        { return unit_id; }

  auto get_dvd_list() -> vector<DVD>
  {
    if (dvd_list.empty()) for (int i=0; i<10; i++)
    {
      DVD d(str(format("dvd no: %d") % i));
      dvd_list.push_back(d);
    }

    return dvd_list;
  }

  vector<DVD> dvd_list;

  int   status;
  int   request;
  void* data;
  char  pending;
  int   unit_id;
  void* stats;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void dvd_free(mrb_state* mrb, void* p)
{
  cout << "c: dvd_free is called\n";

  auto dvd = (DVD*)p;
  delete dvd;
}

static struct mrb_data_type dvd_type = {
  "DVD", dvd_free
};

mrb_value dvd_initialize(mrb_state* mrb, mrb_value self)
{
  auto dvd = (DVD*)mrb_get_datatype(mrb, self, &dvd_type);
  if (dvd != nullptr)
    dvd_free(mrb, dvd);

  // REMARK ci : call info
  // ci->argc should be "1" in this class
  if (mrb->ci->argc == 0)
  {
    return mrb_nil_value();
  }

  mrb_value name;
  mrb_get_args(mrb, "S", &name);

  dvd = new DVD(RSTRING_PTR(name));

  DATA_PTR(self)  = (void*)dvd;
  DATA_TYPE(self) = &dvd_type;

  return self;
}

mrb_value dvd_set_name(mrb_state* mrb, mrb_value self)
{
  auto dvd = (DVD*)mrb_get_datatype(mrb, self, &dvd_type);
  if (dvd == nullptr)
  {
    cout << "c: dvd set.name is null\n";
    return mrb_nil_value();
  }

  mrb_value name;
  mrb_get_args(mrb, "S", &name);
  dvd->name = string(RSTRING_PTR(name), RSTRING_LEN(name));

  return self;
}

mrb_value dvd_get_name(mrb_state* mrb, mrb_value self)
{
  mrb_p(mrb, self);

  auto dvd = (DVD*)mrb_get_datatype(mrb, self, &dvd_type);

  if (dvd == nullptr)
  {
    cout << "c: dvd get.name is null\n";
    return mrb_nil_value();
  }

  return mrb_str_new_cstr(mrb, dvd->name.c_str());
}

////////////////////////////////////////////////////////////////////////////////
//
// ruby wrapper for CDJukeBox
// REMARK: time.c, array.c 및 기타 mruby/src source 참고
//
// MRI ruby 예제의 jb_alloc과 jb_initialize를 합치다.
//
////////////////////////////////////////////////////////////////////////////////
void jb_free(mrb_state* mrb, void* p)
{
  cout << "c: jb_free is called\n";

  auto jukebox = (CDJukeBox*)p;
  delete jukebox;
}

static struct mrb_data_type jukebox_type = {
  "CDJukeBox", jb_free
};

mrb_value jb_initialize(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukeBox*)mrb_get_datatype(mrb, self, &jukebox_type);
  if (jb != nullptr)
    jb_free(mrb, jb);

  // REMARK ci : call info
  if (mrb->ci->argc == 0)
  {
    // error, ci->argc should be "1" in this class
    return mrb_nil_value();
  }

  mrb_int unit;
  mrb_get_args(mrb, "i", &unit);

  jb = new CDJukeBox(unit);

  DATA_PTR(self)  = (void*)jb;
  DATA_TYPE(self) = &jukebox_type;

  return self;

  /** /
  return mrb_obj_value(Data_Wrap_Struct(mrb, 
        mrb_class_ptr(self),
        &jukebox_type, (void*)jb));
  / **/
}

mrb_value jb_seek(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukeBox*)mrb_get_datatype(mrb, self, &jukebox_type);

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
  auto jb = (CDJukeBox*)mrb_get_datatype(mrb, self, &jukebox_type);
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
  auto jb = (CDJukeBox*)mrb_get_datatype(mrb, self, &jukebox_type);
  jb->unit(int(id));

  return self;
}

mrb_value jb_avg_seek_time(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukeBox*)mrb_get_datatype(mrb, self, &jukebox_type);
  return mrb_float_value(jb->avg_seek_time());
}

mrb_value jb_get_dvd_list(mrb_state* mrb, mrb_value self)
{
  auto jb = (CDJukeBox*)mrb_get_datatype(mrb, self, &jukebox_type);
  auto dl = jb->get_dvd_list();
  auto ar = mrb_ary_new_capa(mrb, dl.size());

  for (auto it=dl.begin(); it!=dl.end(); ++it)
  {
    mrb_value argv[1];
    argv[0] = mrb_str_new_cstr(mrb, it->name.c_str()); 
    mrb_p(mrb, argv[0]);

    auto rr = mrb_class_new_instance(mrb, 1, argv, mrb_class_obj_get(mrb, "DVD"));
    mrb_p(mrb, rr);
    
    mrb_ary_push(mrb, ar, rr);
  }

  return ar;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

void init_dvd(mrb_state* mrb)
{
  auto dvd = mrb_define_class(mrb, "DVD", mrb->object_class);

  MRB_SET_INSTANCE_TT(dvd, MRB_TT_DATA);
  mrb_define_method(mrb, dvd, "initialize", dvd_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, dvd, "name",  dvd_get_name, ARGS_NONE());
  mrb_define_method(mrb, dvd, "name=", dvd_set_name, ARGS_REQ(1));
}

void init_jukebox(mrb_state* mrb)
{
  // We don't have to pull this jb out of this scope, i.e. global variable
  auto jb = mrb_define_class(mrb, "CDJukeBox", mrb->object_class);

  MRB_SET_INSTANCE_TT(jb, MRB_TT_DATA); // REMARK: confer array.c

  mrb_define_method(mrb, jb, "initialize", jb_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "seek", jb_seek, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "avg_seek_time", jb_avg_seek_time, ARGS_NONE());
  mrb_define_method(mrb, jb, "unit",  jb_get_unit, ARGS_NONE());
  mrb_define_method(mrb, jb, "unit=", jb_set_unit, ARGS_REQ(1));

  mrb_define_method(mrb, jb, "dvd_list", jb_get_dvd_list, ARGS_NONE());
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
  init_dvd(mrb);
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
