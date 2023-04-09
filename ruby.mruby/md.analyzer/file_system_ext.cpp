#include "file_system_ext.h"
#include "file_system.h"

#include "mruby_basis.h"

#include <iostream>
#include <cassert>

#include <boost/format.hpp>

using namespace std;
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void fs_free(mrb_state* mrb, void* p)
{
  auto fs = (FileSystem*)p;
  cout << str(format("c: fs_free at %x, name: %s\n") % p % fs->name());
}

static struct mrb_data_type fs_type = {
  "FileSystem", fs_free
};

auto fs_wrap(mrb_state* mrb, FileSystem* fs) -> mrb_value 
{
  auto cls = mrb_class_get(mrb, "FileSystem");
  return mrb_obj_value(Data_Wrap_Struct(mrb, cls, &fs_type, (void*)fs));
}

auto fs_initialize(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  auto fs = DATA_CHECK_GET_PTR(mrb, self, &fs_type, class FileSystem);
  if (fs != nullptr)
    fs_free(mrb, fs);

  int argc = mrb_get_argc(mrb);
  if (argc == 0)
    return mrb_nil_value();

  auto name = mrb_get_arg1(mrb);
  //mrb_value name; mrb_get_args(mrb, "S", &name);
  DATA_PTR(self)  = new FileSystem(RSTRING_PTR(name));
  DATA_TYPE(self) = &fs_type;

  return self;
}

auto fs_get_name(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  auto fs = DATA_CHECK_GET_PTR(mrb, self, &fs_type, class FileSystem);
  assert(fs);

  return mrb_str_new_cstr(mrb, fs->name().c_str());
}

auto init_file_system(mrb_state* mrb) -> RClass*
{
  auto fs = mrb_define_class(mrb, "FileSystem", mrb->object_class);

  MRB_SET_INSTANCE_TT(fs, MRB_TT_DATA);

  mrb_define_method(mrb, fs, "initialize", fs_initialize, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, fs, "name", fs_get_name, MRB_ARGS_NONE());

  return fs;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
