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
CFile::CFile(string const& p)
  : _path(p)
{
  _name = fs::path(_path).filename().string();
  _size = 1234;
}

CFile::~CFile()
{
}

auto CFile::name() -> string
{
  return _name;
}

auto CFile::path() -> string
{
  return _path;
}

auto CFile::size() -> int64_t
{
  return _size;
}

auto CFile::parent() -> string
{
  return fs::path(_path).parent_path().string();
}

auto CFile::deleted() -> bool
{
  return false;
}

auto CFile::seek(int pos) -> void
{
}

auto CFile::save_to(string const& res) -> bool
{
  cout << "save_to: " << res << endl;

  return true;
}

auto CFile::read(uint32_t size) -> vector<uint8_t>
{
  vector<uint8_t> v;

  return v;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
