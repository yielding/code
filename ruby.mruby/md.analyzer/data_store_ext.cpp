#include "data_store_ext.h"
#include "data_store.h"
#include "file_system.h"
#include "file_system_ext.h"

#include "mrubybind.hpp"

#include <format>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// the data_store singleton is host-owned: wrapped borrowed, published as $ds.
// file_systems builds a Hash keyed by name — the GC arena is restored per
// element so large stores do not pile up uncollectable temporaries.
//
////////////////////////////////////////////////////////////////////////////////
namespace 
{
  auto ds_file_systems(mrb_state* mrb, mrb_value self) -> mrb_value
  {
    auto ds   = mrubybind::klass<data_store>::unwrap(mrb, self);
    auto& fss = ds->get_file_systems();
    auto hs   = mrb_hash_new_capa(mrb, static_cast<mrb_int>(fss.size()));

    for (auto fs: fss)
    {
      mrubybind::arena_guard guard(mrb);
      auto name = fs->name();
      auto key  = mrb_str_new(mrb, name.data(), static_cast<mrb_int>(name.size()));
      mrb_hash_set(mrb, hs, key, fs_wrap(mrb, fs));
    }

    return hs;
  }

  auto ds_desc(mrb_state* mrb, mrb_value self) -> mrb_value
  {
    auto ds   = mrubybind::klass<data_store>::unwrap(mrb, self);
    auto sz   = ds->get_file_systems().size();
    auto desc = format("DataStore for {} ({} file systems ({} nodes), {} models)",
                       ds->device_name(), sz, 1024, 2048);

    return mrb_str_new(mrb, desc.data(), static_cast<mrb_int>(desc.size()));
  }
}

auto init_data_store(mrb_state* mrb) -> void
{
  mrubybind::klass<data_store>::define(mrb, "MD", "DataStore")
    .method_raw("file_systems", ds_file_systems, MRB_ARGS_NONE())
    .method_raw("desc", ds_desc, MRB_ARGS_NONE());

  auto ds = mrubybind::klass<data_store>::wrap(mrb, &data_store::instance(), false);
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$ds"), ds);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
