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
CFile::CFile(string const& p)
  : _path(p)
{
  auto ph = std::filesystem::path(_path);

  _name = ph.filename().string();
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
