#include "hfs.h"

#include <fstream>
#include <string>

using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSFile;
class CatalogTree;

class HFSVolume
{
public:
  HFSVolume(int64_t offset=0);
  ~HFSVolume();

public:
  bool open(char const* filename);

  auto read(int64_t offset, size_t sz) -> ByteBuffer;
  auto read(int64_t offset, uint8_t* buffer, size_t sz) -> size_t;

  auto list_folder_contents(std::string const& path) -> void;
  auto read_file(std::string const& path, std::string const& mp) -> bool;
  
public:
  auto block_size() -> uint32_t 
  { 
    return m_block_size; 
  }

private:
  int64_t  m_offset;
  uint32_t m_block_size;

  HFSPlusVolumeHeader m_header;

private:
  HFSFile*     m_catalog_file;
  CatalogTree* m_catalog_tree;

private:
  bool         m_opened;
  std::fstream m_stream;
  std::string  m_filename;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////