#include "file_ext.h"
#include "file.h"

#include "mruby_basis.h"

#include <iostream>
#include <cassert>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void fi_free(mrb_state* mrb, void* p)
{
  auto fi = (File*)p;
  if (fi != nullptr)
    delete fi;
}

static struct mrb_data_type fi_type = 
{
  "File", fi_free
};

mrb_value fi_initialize(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_get_datatype(mrb, self, &fi_type);
  if (fi != nullptr)
    fi_free(mrb, fi);

  if (mrb->ci->argc == 0)
    return mrb_nil_value();

  mrb_value path; mrb_get_args(mrb, "S", &path);
  DATA_PTR(self)  = new File(RSTRING_PTR(path));
  DATA_TYPE(self) = &fi_type;

  return self;
}

mrb_value fi_get_name(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_check_datatype(mrb, self, &fi_type); assert(fi);
  return mrb_str_new2(mrb, fi->name().c_str());
}

mrb_value fi_get_path(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_check_datatype(mrb, self, &fi_type); assert(fi);
  return mrb_str_new2(mrb, fi->path().c_str());
}

mrb_value fi_get_parent(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_check_datatype(mrb, self, &fi_type); assert(fi);
  return mrb_str_new2(mrb, fi->parent().c_str());
}

mrb_value fi_get_size(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_check_datatype(mrb, self, &fi_type); assert(fi);
  return mrb_fixnum_value(fi->size());
}

mrb_value fi_deleted(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_check_datatype(mrb, self, &fi_type); assert(fi);
  return fi->deleted()
    ? mrb_true_value()
    : mrb_false_value();
}

mrb_value fi_save_to(mrb_state* mrb, mrb_value self)
{
  auto fi = (File*)mrb_check_datatype(mrb, self, &fi_type); assert(fi);
  mrb_value path; mrb_get_args(mrb, "S", &path);
  auto res = fi->save_to(RSTRING_PTR(path));
  return res ? mrb_true_value()
             : mrb_false_value();
}

void init_file(mrb_state* mrb)
{
  auto fi = mrb_define_class(mrb, "File", mrb->object_class);
  MRB_SET_INSTANCE_TT(fi, MRB_TT_DATA);

  mrb_define_method(mrb, fi, "initialize", fi_initialize, ARGS_REQ(1));
  mrb_define_method(mrb, fi, "name",       fi_get_name,   ARGS_NONE());
  mrb_define_method(mrb, fi, "path",       fi_get_path,   ARGS_NONE());
  mrb_define_method(mrb, fi, "parent",     fi_get_parent, ARGS_NONE());
  mrb_define_method(mrb, fi, "size",       fi_get_size,   ARGS_NONE());
  mrb_define_method(mrb, fi, "deleted",    fi_deleted,    ARGS_NONE());
  mrb_define_method(mrb, fi, "save_to",    fi_save_to,    ARGS_REQ(1));
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
