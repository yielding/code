#include "data_store_ext.h"
#include "data_store.h"

#include <mruby.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/variable.h>

#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void ds_free(mrb_state* mrb, void* p)
{
  cout << "ds_free is called\n";

  //auto ds = (DataStore*)p;
  //delete ds;
}

struct mrb_data_type ds_type = {
  "DataStore", ds_free
};

mrb_value 
ds_open(mrb_state* mrb, mrb_value self)
{
  auto instance = &DataStore::instance();
  DATA_PTR(self) = (void*)instance;
  DATA_TYPE(self) = &ds_type;

  return self;
}

mrb_value 
ds_get_file_systems(mrb_state* mrb, mrb_value self)
{
  //auto instance = &DataStore::instance();
  auto ds = (DataStore*)mrb_get_datatype(mrb, self, &ds_type);
  if (ds == nullptr)
  {
    cout << "error in getting DataStore pointer";
    ds = &DataStore::instance();
  }

  // ds->get_file_systems();

  cout << "get_file_systems is called";

  return mrb_nil_value();
}

void init_data_store(mrb_state* mrb)
{
  auto ds = mrb_define_class(mrb, "DataStore", mrb->object_class);

  MRB_SET_INSTANCE_TT(ds, MRB_TT_DATA);

  //mrb_undef_method(mrb, ds, "new");  // new를 호출할 수 없게 만든다.
  mrb_define_class_method(mrb, ds, "initialize", ds_open, ARGS_NONE());
  mrb_define_method(mrb, ds, "file_systems", ds_get_file_systems, ARGS_NONE());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
