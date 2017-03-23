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
  auto fi = (CFile*)p;
  if (fi != nullptr)
    delete fi;
}

static const struct mrb_data_type fi_type = {
  "File", fi_free
};

static mrb_value fi_initialize(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_CHECK_GET_PTR(mrb, self, &fi_type, class File);
  if (fi != nullptr)
    fi_free(mrb, fi);

  /*
  if (mrb->c->ci->argc == 0)
    return mrb_nil_value();
  */

  DATA_TYPE(self) = NULL;

  mrb_value path; mrb_get_args(mrb, "S", &path);
  DATA_TYPE(self) = &fi_type;
  DATA_PTR(self)  = new CFile(RSTRING_PTR(path));

  return self;
}

static mrb_value fi_get_name(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_GET_PTR(mrb, self, &fi_type, class CFile); assert(fi);

  return mrb_str_new_cstr(mrb, fi->name().c_str());
}

static mrb_value fi_get_path(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_GET_PTR(mrb, self, &fi_type, class CFile); assert(fi);

  return mrb_str_new_cstr(mrb, fi->path().c_str());
}

static mrb_value fi_get_parent(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_GET_PTR(mrb, self, &fi_type, class CFile); assert(fi);

  return mrb_str_new_cstr(mrb, fi->parent().c_str());
}

static mrb_value fi_get_size(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_GET_PTR(mrb, self, &fi_type, class CFile); assert(fi);

  return mrb_fixnum_value(fi->size());
}

static mrb_value fi_deleted(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_GET_PTR(mrb, self, &fi_type, class CFile); assert(fi);

  return fi->deleted()
    ? mrb_true_value()
    : mrb_false_value();
}

static mrb_value fi_save_to(mrb_state* mrb, mrb_value self)
{
  auto fi = DATA_GET_PTR(mrb, self, &fi_type, class CFile); assert(fi);
  mrb_value path; mrb_get_args(mrb, "S", &path);
  auto res = fi->save_to(RSTRING_PTR(path));

  return res ? mrb_true_value()
             : mrb_false_value();
}

auto init_file(mrb_state* mrb) -> RClass*
{
  auto fi = mrb_define_class(mrb, "CFile", mrb->object_class);
  MRB_SET_INSTANCE_TT(fi, MRB_TT_DATA);

  mrb_define_method(mrb, fi, "initialize", fi_initialize, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, fi, "name",       fi_get_name,   MRB_ARGS_NONE());
  mrb_define_method(mrb, fi, "path",       fi_get_path,   MRB_ARGS_NONE());
  mrb_define_method(mrb, fi, "parent",     fi_get_parent, MRB_ARGS_NONE());
  mrb_define_method(mrb, fi, "size",       fi_get_size,   MRB_ARGS_NONE());
  mrb_define_method(mrb, fi, "deleted?",   fi_deleted,    MRB_ARGS_NONE());
  mrb_define_method(mrb, fi, "save_to",    fi_save_to,    MRB_ARGS_REQ(1));

  return fi;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
