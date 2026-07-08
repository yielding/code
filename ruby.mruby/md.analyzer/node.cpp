#include "node.h"

#include <iostream>
#include <filesystem>

using namespace std;
      namespace fs = filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
node::node(string const& p)
  : _path(p)
{
  auto ph = std::filesystem::path(_path);

  _name = ph.filename().string();
  _size = 1234;
}

node::~node()
{
}

auto node::name() -> string
{
  return _name;
}

auto node::path() -> string
{
  return _path;
}

auto node::size() -> int64_t
{
  return _size;
}

auto node::parent() -> string
{
  return fs::path(_path).parent_path().string();
}

auto node::deleted() -> bool
{
  return false;
}

auto node::seek(int pos) -> void
{
}

auto node::save_to(string const& res) -> bool
{
  cout << "save_to: " << res << endl;

  return true;
}

auto node::read(uint32_t size) -> vector<uint8_t>
{
  vector<uint8_t> v;

  return v;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
