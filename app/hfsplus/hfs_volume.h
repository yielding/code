#include "hfs.h"

#include <fstream>

using namespace utility::hex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSVolume
{
public:
  HFSVolume(int64_t offset=0);

public:
  bool open(char const* filename);

  ByteBuffer read(int64_t offset, size_t sz);
  size_t     read(int64_t offset, uint8_t* buffer, size_t sz);


public:
  size_t     block_size() { return m_block_size; }


private:
  int64_t  m_offset;
  uint32_t m_block_size;

  HFSPlusVolumeHeader m_header;

private:
  std::fstream m_stream;
  std::string  m_filename;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
