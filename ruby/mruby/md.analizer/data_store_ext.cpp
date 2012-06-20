#include "data_store.h"
#include "file_system.h"
#include "file_system_ext.h"

#include "mruby_basis.h"

#include <cassert>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void ds_free(mrb_state* mrb, void* p)
{
  auto ds = (DataStore*)p;
  if (ds != nullptr)
    delete ds;
}

struct mrb_data_type ds_type = 
{
  "DataStore", ds_free
};

mrb_value 
ds_get_file_systems(mrb_state* mrb, mrb_value self)
{
  auto ds = (DataStore*)mrb_check_datatype(mrb, self, &ds_type);
  assert(ds);

  auto fss = ds->get_file_systems();
  auto arr = mrb_ary_new_capa(mrb, fss.size());
  for (auto it = fss.begin(); it != fss.end(); ++it)
  {
    auto fs_p = *it;
    auto fs_r = fs_wrap(mrb, fs_p);
    mrb_ary_push(mrb, arr, fs_r);
  }

  return arr;
}

void init_data_store(mrb_state* mrb)
{
  auto ds = mrb_define_class(mrb, "DataStore", mrb->object_class);

  MRB_SET_INSTANCE_TT(ds, MRB_TT_DATA);

  auto ds_p = &DataStore::instance();
  auto ds_r = mrb_obj_value(Data_Wrap_Struct(mrb, ds, &ds_type, (void*)ds_p));
  mrb_gv_set(mrb, mrb_intern(mrb, "$ds"), ds_r);
  mrb_define_method(mrb, ds, "file_systems", ds_get_file_systems, ARGS_NONE());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
