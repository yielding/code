#include "file_ext.h"
#include "file.h"

#include "mrubybind.hpp"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// arguments are parsed from the C++ signatures ("S", "i", ...), so a bad
// script argument (CFile.new(123)) raises TypeError instead of crashing
// the host. read() returns its bytes as a binary Ruby string.
//
////////////////////////////////////////////////////////////////////////////////
auto init_file(mrb_state* mrb) -> void
{
  mrubybind::Klass<CFile>::define(mrb, "CFile")
    .ctor<string>()
    .method<&CFile::name>("name")
    .method<&CFile::path>("path")
    .method<&CFile::parent>("parent")
    .method<&CFile::size>("size")
    .method<&CFile::deleted>("deleted?")
    .method<&CFile::seek>("seek")
    .method<&CFile::read>("read")
    .method<&CFile::save_to>("save_to");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
