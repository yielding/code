#include "file.h"

#include <boost/filesystem.hpp>
#include <iostream>

using namespace std;
using namespace boost;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
File::File(std::string const& p)
  : _path(p)
{
  _name = fs::path(_path).filename().string();
  _size = 1234;
}

File::~File()
{
}

auto File::name() -> string
{
  return _name;
}

auto File::path() -> string
{
  return _path;
}

auto File::size() -> int64_t
{
  return _size;
}

auto File::parent() -> string
{
  return fs::path(_path).parent_path().string();
}

auto File::deleted() -> bool
{
  return false;
}

auto File::seek(int pos) -> void
{
}

auto File::save_to(std::string const& res) -> bool
{
  cout << "save_to: " << res << endl;

  return true;
}

auto File::read(uint32_t size) -> std::vector<uint8_t>
{
  vector<uint8_t> v;

  return v;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
