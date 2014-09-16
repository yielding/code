#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <fstream>

#include <boost/format.hpp>

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/compile.h>
#include <mruby/dump.h>

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
  DVD(string n="") : name(n) {}
  DVD(DVD const& rhs) 
  {
    if (this != &rhs)
    {
        name = rhs.name;
    }
  }

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

  double avg_seek_time() { return 1.23;    }
  void unit(int id)      { unit_id = id;   }
  int unit()             { return unit_id; }

  auto get_dvd_list() -> vector<DVD>&
  {
    if (dvd_list.empty()) for (int i=0; i<10; i++)
    {
      DVD d(str(format("dvd no: %d") % i));
      dvd_list.push_back(d);
    }

    return dvd_list;
  }

  auto get_user_list() -> map<string, string>&
  {
    user_list["leech"] = "sanbon";
    user_list["leeks"] = "ansan";
    //user_list["leech"] = "군포시 광정동 세종아파트";
    //user_list["leeks"] = "안산시 somewhere";

    return user_list;
  }

  vector<DVD> dvd_list;
  map<string, string> user_list;

  int   status;
  //int   request;
  //void* data;
  //char  pending;
  int   unit_id;
  //void* stats;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct MusicStore
{
  auto make_jukebox(int id) -> CDJukeBox*
  {
    auto jb = new CDJukeBox(id);
    return jb;
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void dvd_free(mrb_state* mrb, void* p)
{
  auto dvd = (DVD*)p;
  if (dvd)
  {
    cout << str(format("c:dvd_free at %x, name %s\n") % p % dvd->name);
    if (dvd != nullptr)
    {
      delete dvd;
      dvd = nullptr;
    }
  }
}

static const struct mrb_data_type dvd_type = 
{
  "DVD", dvd_free
};

mrb_value dvd_initialize(mrb_state* mrb, mrb_value self)
{
    auto dvd = DATA_CHECK_GET_PTR(mrb, self, &dvd_type, class DVD);
    if (dvd != nullptr)
    {
        cout << "dvd instance is not nil\n";
        dvd_free(mrb, dvd);
    }

    /*
    if (mrb->c->ci->argc == 0)
        return mrb_nil_value();
    */

    mrb_value name; 
    mrb_get_args(mrb, "S", &name);
    DATA_PTR(self)  = new DVD(RSTRING_PTR(name));
    DATA_TYPE(self) = &dvd_type;

    return self;
}

mrb_value dvd_set_name(mrb_state* mrb, mrb_value self)
{
  auto dvd = DATA_CHECK_GET_PTR(mrb, self, &dvd_type, class DVD);
  if (dvd == nullptr)
  {
    cout << "c: dvd set.name is null\n";
    return mrb_nil_value();
  }

  mrb_value name;
  mrb_get_args(mrb, "S", &name);
  dvd->name = string(RSTRING_PTR(name));

  return self;
}

mrb_value dvd_get_name(mrb_state* mrb, mrb_value self)
{
  auto dvd = DATA_CHECK_GET_PTR(mrb, self, &dvd_type, class DVD);

  if (dvd == nullptr)
  {
    cout << "c: dvd get.name is nullptr\n";
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
static void jb_free(mrb_state* mrb, void* p)
{
  cout << "c: jb_free is called\n";
  auto jukebox = (CDJukeBox*)p;
  if (jukebox != nullptr)
    delete jukebox;
}

static const struct mrb_data_type jukebox_type = 
{
  "CDJukeBox", jb_free
};

static mrb_value 
jb_initialize(mrb_state* mrb, mrb_value self)
{
  auto jb = DATA_CHECK_GET_PTR(mrb, self, &jukebox_type, class CDJukeBox);
  if (jb != nullptr)
    jb_free(mrb, jb);

  mrb_int unit;   
  mrb_get_args(mrb, "i", &unit);

  DATA_PTR(self)  = new CDJukeBox(unit);
  DATA_TYPE(self) = &jukebox_type;

  return self;
}

static mrb_value 
jb_seek(mrb_state* mrb, mrb_value self)
{
  auto jb = DATA_CHECK_GET_PTR(mrb, self, &jukebox_type, class CDJukeBox);
  assert(jb);

  mrb_int disk, track;
  mrb_value blk;
  mrb_get_args(mrb, "ii&", &disk, &track, &blk);
  jb->seek(disk, track);
  for (int p=0; p<100; p+=10)
    mrb_yield(mrb, blk, mrb_fixnum_value(p));

  return self;
}

static mrb_value 
jb_get_unit(mrb_state* mrb, mrb_value self)
{
  auto jb = DATA_GET_PTR(mrb, self, &jukebox_type, class CDJukeBox);
  assert(jb);

  return mrb_fixnum_value(jb->unit());
}

static mrb_value 
jb_set_unit(mrb_state* mrb, mrb_value self)
{
  auto jb = DATA_CHECK_GET_PTR(mrb, self, &jukebox_type, class CDJukeBox);
  assert(jb);

  mrb_int id; mrb_get_args(mrb, "i", &id);
  jb->unit(int(id));

  return self;
}

mrb_value jb_avg_seek_time(mrb_state* mrb, mrb_value self)
{
  auto jb = DATA_CHECK_GET_PTR(mrb, self, &jukebox_type, class CDJukeBox);
  assert(jb);

  return mrb_float_value(mrb, jb->avg_seek_time());
}

mrb_value jb_get_dvd_list(mrb_state* mrb, mrb_value self)
{
  auto jb = DATA_CHECK_GET_PTR(mrb, self, &jukebox_type, class CDJukeBox);
  assert(jb);

  // REMARK: getting with reference is very important
  auto& dl = jb->get_dvd_list();
  auto  ar = mrb_ary_new_capa(mrb, dl.size());
  auto cls = mrb_class_get(mrb, "DVD");

  for (auto it=dl.begin(); it!=dl.end(); ++it)
  {
    auto pdvd = new DVD(*it);
    auto dvd = mrb_obj_value(Data_Wrap_Struct(mrb, cls, &dvd_type, (void*) pdvd));
    // auto dvd = mrb_obj_value(Data_Wrap_Struct(mrb, cls, &dvd_type, (void*) &*it));

    mrb_ary_push(mrb, ar, dvd);
  }

  return ar;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void ms_free(mrb_state* mrb, void* p)
{
  cout << "ms_free is called\n";

  auto store = (MusicStore*)p;
  if (store != nullptr)
    delete store;
}

static struct mrb_data_type mstore_type = 
{
  "MusicStore", ms_free
};

mrb_value ms_get_jukebox(mrb_state* mrb, mrb_value val)
{
  auto ms = DATA_CHECK_GET_PTR(mrb, val, &mstore_type, class MusicStore);
  assert(ms);

  mrb_int id; mrb_get_args(mrb, "i", &id);
  auto cls = mrb_class_get(mrb, "CDJukeBox");
  auto jb  = ms->make_jukebox(id);

  return mrb_obj_value(Data_Wrap_Struct(mrb, cls, &jukebox_type, (void*) jb));
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
  auto jb = mrb_define_class(mrb, "CDJukeBox", mrb->object_class);

  MRB_SET_INSTANCE_TT(jb, MRB_TT_DATA);

  mrb_define_method(mrb, jb, "initialize", jb_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "seek", jb_seek, ARGS_REQ(1));
  mrb_define_method(mrb, jb, "avg_seek_time", jb_avg_seek_time, ARGS_NONE());
  mrb_define_method(mrb, jb, "unit",  jb_get_unit, ARGS_NONE());
  mrb_define_method(mrb, jb, "unit=", jb_set_unit, ARGS_REQ(1));

  mrb_define_method(mrb, jb, "dvd_list", jb_get_dvd_list, ARGS_NONE());
}

MusicStore* g_ms = nullptr;

void init_music_store(mrb_state* mrb)
{
  auto ms = mrb_define_class(mrb, "MusicStore", mrb->object_class);

  MRB_SET_INSTANCE_TT(ms, MRB_TT_DATA);
  mrb_define_method(mrb, ms, "create_jukebox", ms_get_jukebox, ARGS_REQ(1));

  g_ms = new MusicStore;

  auto store = mrb_obj_value(
      Data_Wrap_Struct(mrb, ms, &mstore_type, (void*)g_ms));
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$mstore"), store);
}

}

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
    while (getline(ifs, line)) code += line + "\n";
    return code;
  };

  auto mrb = mrb_open();
  init_dvd(mrb);
  init_jukebox(mrb);
  init_music_store(mrb);

  auto code = load_script("myscript.rb");

  auto c = mrbc_context_new(mrb);

  auto p = mrb_parse_string(mrb, code.c_str(), c);
  // auto p = mrb_parser_new(mrb);
  // p->s    = code.c_str();
  // p->send = code.c_str() + code.length();
  // p->lineno = 1;
  // mrb_parser_parse(p, c);

  auto proc = mrb_generate_code(mrb, p);
  mrb_run(mrb, proc, mrb_top_self(mrb));
  if (mrb->exc)
  {
    mrb_p(mrb, mrb_obj_value(mrb->exc));
    mrb->exc = 0;
  }

  mrb_parser_free(p);
  mrbc_context_free(mrb, c);
  mrb_close(mrb);

  return 0;
}
