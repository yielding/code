#include "data_store.h"
#include "file_system.h"

#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
data_store::data_store()
{
  char const* names[] = { "hfs", "hfs+", "hfsx", nullptr };

  for (int i=0; names[i]; ++i)
    _filesystems.push_back(new file_system(names[i]));
}

data_store::~data_store()
{
  cout << "c: the destructor of singleton data_store is called\n";
  cout << "delete filesystem object only once at here\n";

  for (auto fs: _filesystems) 
    delete fs;
}

auto data_store::get_file_systems() -> std::vector<file_system*>&
{
  return _filesystems;
}

auto data_store::device_name() -> string
{
  return "iPhone 4S";
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
