#include "file_ext.h"
#include "file.h"

#include "mrubybind.hpp"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// arguments are parsed from the C++ signatures ("S", "i", ...), so a bad
// script argument (MD::File.new(123)) raises TypeError instead of crashing
// the host. read() returns its bytes as a binary Ruby string.
//
////////////////////////////////////////////////////////////////////////////////
auto init_file(mrb_state* mrb) -> void
{
  mrubybind::klass<file>::define(mrb, "MD", "File")
    .ctor<string>()
    .method<&file::name>("name")
    .method<&file::path>("path")
    .method<&file::parent>("parent")
    .method<&file::size>("size")
    .method<&file::deleted>("deleted?")
    .method<&file::seek>("seek")
    .method<&file::read>("read")
    .method<&file::save_to>("save_to");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
