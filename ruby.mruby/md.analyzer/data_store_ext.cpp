#include "data_store_ext.h"
#include "data_store.h"
#include "file_system.h"
#include "file_system_ext.h"

#include "mrubybind.hpp"

#include <boost/format.hpp>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// the DataStore singleton is host-owned: wrapped borrowed, published as $ds.
// file_systems builds a Hash keyed by name — the GC arena is restored per
// element so large stores do not pile up uncollectable temporaries.
//
////////////////////////////////////////////////////////////////////////////////
namespace {

  auto ds_file_systems(mrb_state* mrb, mrb_value self) -> mrb_value
  {
    auto ds   = mrubybind::Klass<DataStore>::unwrap(mrb, self);
    auto& fss = ds->get_file_systems();
    auto hs   = mrb_hash_new_capa(mrb, static_cast<mrb_int>(fss.size()));

    for (auto fs: fss)
    {
      mrubybind::ArenaGuard guard(mrb);
      auto name = fs->name();
      auto key  = mrb_str_new(mrb, name.data(), static_cast<mrb_int>(name.size()));
      mrb_hash_set(mrb, hs, key, fs_wrap(mrb, fs));
    }

    return hs;
  }

  auto ds_desc(mrb_state* mrb, mrb_value self) -> mrb_value
  {
    auto ds   = mrubybind::Klass<DataStore>::unwrap(mrb, self);
    auto sz   = ds->get_file_systems().size();
    auto desc = str(boost::format("DataStore for %s (%d file systems (%d nodes), %d models)")
                    % ds->device_name() % sz % 1024 % 2048);

    return mrb_str_new(mrb, desc.data(), static_cast<mrb_int>(desc.size()));
  }

}

auto init_data_store(mrb_state* mrb) -> void
{
  mrubybind::Klass<DataStore>::define(mrb, "DataStore")
    .method_raw("file_systems", ds_file_systems, MRB_ARGS_NONE())
    .method_raw("desc", ds_desc, MRB_ARGS_NONE());

  auto ds = mrubybind::Klass<DataStore>::wrap(mrb, &DataStore::instance(), false);
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$ds"), ds);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
