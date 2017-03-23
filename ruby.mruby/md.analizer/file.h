#ifndef FILE_H
#define FILE_H

#include <cstdint>
#include <string>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class CFile
{
public:
  CFile(std::string const& path);
  ~CFile();

public: // query
  auto name() -> std::string;
  auto path() -> std::string;
  auto parent() -> std::string;
  auto size()    -> int64_t;
  auto deleted() -> bool;

public: // I/O
  auto seek(int pos) -> void;
  auto read(uint32_t size) -> std::vector<uint8_t>;
  auto save_to(std::string const& res) -> bool;

private:
  std::string _name;
  std::string _path;
  int64_t _size;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

#endif
