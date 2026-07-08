#include "node_ext.h"
#include "node.h"

#include "mrubybind.hpp"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// arguments are parsed from the C++ signatures ("S", "i", ...), so a bad
// script argument (MD::Node.new(123)) raises TypeError instead of crashing
// the host. read() returns its bytes as a binary Ruby string.
//
////////////////////////////////////////////////////////////////////////////////
auto init_node(mrb_state* mrb) -> void
{
  mrubybind::klass<node>::define(mrb, "MD", "Node")
    .ctor<string>()
    .method<&node::name>("name")
    .method<&node::path>("path")
    .method<&node::parent>("parent")
    .method<&node::size>("size")
    .method<&node::deleted>("deleted?")
    .method<&node::seek>("seek")
    .method<&node::read>("read")
    .method<&node::save_to>("save_to");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
