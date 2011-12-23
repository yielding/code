#include "hfs_volume.h"
#include <cassert>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HFSVolume::HFSVolume(int64_t offset)
  : m_offset(offset)
{
}

bool HFSVolume::open(char const* filename)
{
  m_stream.open(filename, ios_base::in | ios_base::out | ios_base::binary);
  if (!m_stream.is_open())
    return false;

  m_filename = string(filename);

  auto b = read(1024, sizeof(HFSPlusVolumeHeader));
  m_header.read_from(b);

  m_block_size = m_header.blockSize;


  // TODO here
  // m_allocation_file
  

  return true;
}

ByteBuffer HFSVolume::read(int64_t offset, size_t sz)
{
  ByteBuffer b(sz);
  auto rd = this->read(offset, (uint8_t*)b, sz);
  assert(rd == sz);

  return b;
}

size_t HFSVolume::read(int64_t offset, uint8_t* buffer, size_t sz)
{
  m_stream.seekg(offset, ios_base::beg);
  if (m_stream.fail())
    throw runtime_error("HFSVolume::read error");

  m_stream.read((char*)buffer, sz);

  return size_t(m_stream.gcount());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
