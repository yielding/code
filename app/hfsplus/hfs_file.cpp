#include "hfs_file.h"
#include "hfs_volume.h"

#include <boost/filesystem.hpp>
#include <fstream>

using namespace std;
using namespace utility::hex;
      namespace fs = boost::filesystem;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HFSFile::HFSFile(HFSVolume* v, HFSPlusForkData fork, HFSCatalogNodeID fileID, bool deleted)
{
  m_volume       = v;
  m_block_size   = m_volume->block_size();
  m_fileID       = fileID;
  m_total_blocks = fork.totalBlocks;
  m_logical_size = fork.logicalSize;
  m_deleted      = deleted;

  m_extents.clear();
  uint32_t bc = 0;

  for (auto i=0; i<8; i++)
  {
    auto& extent = fork.extents[i];
    m_extents.push_back(extent);
    bc += extent.blockCount;
  }

  // 
  // TODO here
  //
  while (bc != fork.totalBlocks)
  {
    // m_volume->get_extent_overflow_for_file(m_fileID, bc);
  }
}

// NOT Tested
auto HFSFile::read_block(uint32_t nth) -> utility::hex::ByteBuffer
{
  auto bs = m_volume->block_size();
  if (int64_t(nth) * bs > m_logical_size)
    throw std::runtime_error("block out of bounds");

  auto bc = 0;
  for (auto it = m_extents.begin(); it != m_extents.end(); ++it)
  {
    bc += it->blockCount;
    if (nth < bc)
    {
      int64_t lba = it->startBlock + (nth - (bc - it->blockCount));
      // TODO some check
      return m_volume->read(lba * bs, bs);
    }
  }

  return ByteBuffer();
}

auto HFSFile::read_all_to_buffer(bool trunc) -> utility::hex::ByteBuffer
{
  ByteBuffer result;
  for (uint32_t i=0; i<m_total_blocks; i++)
  {
    auto b = read_block(i);
    result.append(b);
  }

  return result;
}

void HFSFile::read_all_to_file(char const* filename, bool trunc)
{
  ofstream ofs;
  ofs.open(filename, ios_base::binary);

  for (uint32_t i=0; i<m_total_blocks; i++)
  {
    auto b = read_block(i);
    ofs.write((char const*)b, m_block_size);
  }

  ofs.close();

  fs::resize_file(filename, m_logical_size);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
