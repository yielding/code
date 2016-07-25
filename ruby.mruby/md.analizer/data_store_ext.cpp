#include "data_store.h"
#include "file_system.h"
#include "file_system_ext.h"

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
void ds_free(mrb_state* mrb, void* p)
{
  auto ds = (DataStore*)p;
  cout << str(format("c: ds_free at %x\n") % p);
  // DataStore is Scott Singleton
  // if (ds != nullptr)
  //   delete ds;
}

struct mrb_data_type ds_type = {
  "DataStore", ds_free
};

auto ds_get_file_systems(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  auto ds = DATA_CHECK_GET_PTR(mrb, self, &ds_type, class DataStore);
  assert(ds);

  auto fss = ds->get_file_systems();
  auto hs  = mrb_hash_new_capa(mrb, fss.size());
  for (auto it = fss.begin(); it != fss.end(); ++it)
  {
    auto fs_p = *it;
    auto key  = mrb_str_new_cstr(mrb, fs_p->name().c_str());
    auto fs_r = fs_wrap(mrb, fs_p);
    mrb_hash_set(mrb, hs, key, fs_r);
  }

  return hs;
}

auto ds_get_description(mrb_state* mrb, mrb_value self) -> mrb_value 
{
  auto ds  = DATA_CHECK_GET_PTR(mrb, self, &ds_type, class DataStore);
  auto fss = ds->get_file_systems();
  auto sz  = fss.size();
  auto desc = str(format("DataStore for %s (%d file systems (%d nodes), %d models)")
        % ds->device_name() % sz % 1024 % 2048);

  return mrb_str_new_cstr(mrb, desc.c_str());
}

auto init_data_store(mrb_state* mrb) -> RClass*
{
  auto ds = mrb_define_class(mrb, "DataStore", mrb->object_class);
  MRB_SET_INSTANCE_TT(ds, MRB_TT_DATA);

  auto ds_p = &DataStore::instance();
  auto ds_r = mrb_obj_value(Data_Wrap_Struct(mrb, ds, &ds_type, (void*)ds_p));

  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$ds"), ds_r);
  mrb_define_method(mrb, ds, "file_systems", ds_get_file_systems, MRB_ARGS_NONE());
  mrb_define_method(mrb, ds, "desc", ds_get_description, MRB_ARGS_NONE());

  return ds;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
