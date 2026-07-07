#include "file_system_ext.h"
#include "file_system.h"

#include "mrubybind.hpp"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// FileSystem instances reach scripts two ways with different owners:
//   - handed out by DataStore  -> host owns them, wrap borrowed
//   - created by FileSystem.new -> the GC owns them (Holder.owned = true)
// mrubybind's Holder keeps the two apart in one class.
//
////////////////////////////////////////////////////////////////////////////////
auto fs_wrap(mrb_state* mrb, FileSystem* fs) -> mrb_value
{
  return mrubybind::Klass<FileSystem>::wrap(mrb, fs, false);
}

auto init_file_system(mrb_state* mrb) -> void
{
  mrubybind::Klass<FileSystem>::define(mrb, "FileSystem")
    .ctor<string>()
    .method<&FileSystem::name>("name")
    .method<&FileSystem::desc>("desc");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
