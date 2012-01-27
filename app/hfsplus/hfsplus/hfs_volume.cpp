#include "hfs_volume.h"
#include "hfs_file.h"
#include "catalog_btree.h"

#include <cassert>
#include <stdexcept>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HFSVolume::HFSVolume(int64_t offset)
  : m_offset(offset), m_opened(false)
{
}

HFSVolume::~HFSVolume()
{
  if (m_opened)
  {
    m_opened = false;
    delete m_catalog_tree;
    delete m_catalog_file;
  }
}

bool HFSVolume::open(char const* filename)
{
  m_stream.open(filename, ios_base::in | ios_base::out | ios_base::binary);
  if (!m_stream.is_open())
    return false;

  m_filename = string(filename);

  try
  {
    // auto b = read(1024, sizeof(HFSPlusVolumeHeader));
    auto b = read(1024, 0x1000);
    m_header.read_from(b);

    if (m_header.signature != 0x4858 &&
        m_header.signature != 0x482B)
        throw std::runtime_error("Not an HFS+ image");
  }
  catch(...)
  {
    throw std::runtime_error("Not an HFS+ image");
  }

  m_block_size = m_header.blockSize;


  // TODO here
  // m_allocation_file
  m_catalog_file = new HFSFile(this, m_header.catalogFile, kHFSCatalogFileID);
  m_catalog_tree = new CatalogTree(m_catalog_file);

  m_opened = true;

  return true;
}

void HFSVolume::list_folder_contents(string const& path)
{
  auto record = m_catalog_tree->get_record_from_path(path);
  
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
