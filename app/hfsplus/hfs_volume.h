#include "hfs.h"

#include <fstream>

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

  ByteBuffer read(int64_t offset, size_t sz);
  size_t     read(int64_t offset, uint8_t* buffer, size_t sz);


public:
  uint32_t   block_size() { return m_block_size; }


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
