#include "file.h"

#include <iostream>
#include <filesystem>

using namespace std;
      namespace fs = filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
file::file(string const& p)
  : _path(p)
{
  auto ph = std::filesystem::path(_path);

  _name = ph.filename().string();
  _size = 1234;
}

file::~file()
{
}

auto file::name() -> string
{
  return _name;
}

auto file::path() -> string
{
  return _path;
}

auto file::size() -> int64_t
{
  return _size;
}

auto file::parent() -> string
{
  return fs::path(_path).parent_path().string();
}

auto file::deleted() -> bool
{
  return false;
}

auto file::seek(int pos) -> void
{
}

auto file::save_to(string const& res) -> bool
{
  cout << "save_to: " << res << endl;

  return true;
}

auto file::read(uint32_t size) -> vector<uint8_t>
{
  vector<uint8_t> v;

  return v;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
