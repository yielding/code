#include "data_store.h"
#include "file_system.h"

#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
DataStore::DataStore()
{
  char const* names[] = { "hfs", "hfs+", "hfsx", nullptr };

  for (int i=0; names[i]; ++i)
    _filesystems.push_back(new FileSystem(names[i]));
}

DataStore::~DataStore()
{
    cout << "c: the destructor of singleton DataStore is called\n";
    cout << "delete filesystem object only once at here\n";
    for (auto it=_filesystems.begin(); it!=_filesystems.end(); ++it)
        delete *it;
}

auto DataStore::get_file_systems() -> std::vector<FileSystem*>&
{
  return _filesystems;
}

auto DataStore::device_name() -> string
{
  return "iPhone 4S";
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
