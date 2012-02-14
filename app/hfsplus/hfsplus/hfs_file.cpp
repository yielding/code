#include "hfs_file.h"
#include "hfs_volume.h"
#include "extents_btree.h"

#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <fstream>

using namespace std;
using namespace utility::hex;
using namespace boost;
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
  // TODO be verified
  //
  while (bc != fork.totalBlocks)
  {
    // 0 == kForkTypeData, 1 == kForkTypeResource
    HFSPlusExtentKey key(0, m_fileID, bc);
    auto overflow = m_volume->get_extents_overflow_for_file(key);
    if (overflow.empty())
    {
      cout << "extents overflow missing, startblock=" << bc << endl;
      break;
    }
    
    for (int i=0; i<8; i++)
    {
      auto& extent = overflow.data.extents[i];
      m_extents.push_back(extent);
      bc += extent.blockCount;
    }
  }
}

HFSFile::~HFSFile()
{
}

auto HFSFile::read_block(uint32_t nth) -> ByteBuffer
{
  if (m_logical_size > 0)
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
        auto buffer = m_volume->read(lba * bs, bs);
        return process_block(lba, buffer, bs);
      }
    }
  }

  return ByteBuffer();
}

auto HFSFile::process_block(int64_t lba, ByteBuffer& buffer, uint32_t bs)
-> ByteBuffer&
{
  return buffer;
}

auto HFSFile::read_all_to_buffer(bool trunc) -> ByteBuffer
{
  ByteBuffer result;
  for (uint32_t i=0; i<m_total_blocks; i++)
  {
    auto b = read_block(i);
    result.append(b);
  }
  
  return trunc 
    ? result.slice(0, uint32_t(m_logical_size))
    : result;
}

void HFSFile::read_all_to_file(string const& filename, string const& point, bool trunc)
{
  string path = ends_with(point, "/")
    ? point + filename
    : point + "/" + filename;

  ofstream ofs;
  ofs.open(path.c_str(), ios_base::binary);

  for (uint32_t i=0; i<m_total_blocks; i++)
  {
    auto b = read_block(i);
    ofs.write((char const*)b, m_block_size);
  }

  ofs.close();

  fs::resize_file(path, m_logical_size);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
