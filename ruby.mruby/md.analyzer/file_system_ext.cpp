#include "file_system_ext.h"
#include "file_system.h"

#include "mrubybind.hpp"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// file_system instances reach scripts two ways with different owners:
//   - handed out by data_store  -> host owns them, wrap borrowed
//   - created by MD::Filesystem.new -> the GC owns them (holder.owned = true)
// mrubybind's holder keeps the two apart in one class.
//
////////////////////////////////////////////////////////////////////////////////
auto fs_wrap(mrb_state* mrb, file_system* fs) -> mrb_value
{
  return mrubybind::klass<file_system>::wrap(mrb, fs, false);
}

auto init_file_system(mrb_state* mrb) -> void
{
  mrubybind::klass<file_system>::define(mrb, "MD", "Filesystem")
    .ctor<string>()
    .method<&file_system::name>("name")
    .method<&file_system::desc>("desc");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
