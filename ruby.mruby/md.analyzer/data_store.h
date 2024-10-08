#pragma once

#include <vector>
#include <string>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class FileSystem;

class DataStore
{
public:
  static DataStore& instance()
  {
    static DataStore singleton;
    return singleton;
  }

  DataStore(const DataStore&) = delete;

  ~DataStore();

public:
  auto get_file_systems() -> std::vector<FileSystem*>&;
  auto device_name()      -> std::string;

private:
  DataStore();

private:
  std::vector<FileSystem*> _filesystems;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
