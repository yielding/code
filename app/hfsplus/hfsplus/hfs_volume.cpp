#include "hfs_volume.h"
#include "hfs_file.h"
#include "extents_btree.h"
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
    delete m_allocation_file;
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
  m_allocation_file = new HFSFile(this, m_header.allocationFile, kHFSAllocationFileID);
  m_allocatioin_bitmap = m_allocation_file->read_all_to_buffer();
  m_extents_file = new HFSFile(this, m_header.extentsFile, kHFSExtentsFileID);
  m_extents_tree = new ExtentsTree(m_extents_file);
  m_catalog_file = new HFSFile(this, m_header.catalogFile, kHFSCatalogFileID);
  m_catalog_tree = new CatalogTree(m_catalog_file);

  // TODO
  //
  // m_metadat_dir = m_catalog_file->metadata_dir_id();
  //
  // auto buffer = m_attribute_file->get_attribute(kHFSRootParentID, "com.apple.system.cprotect", &buffer);
  // if (buffer.size() > 0)
  //   m_cp_root.read_from(buffer);

  m_opened = true;

  return true;
}

auto HFSVolume::get_extents_overflow_for_file(HFSPlusExtentKey const& key)
-> ExtentsLeafRecord
{
  return m_extents_tree->search_extents(key);
}

void HFSVolume::list_folder_contents(string const& path)
{
  auto record = m_catalog_tree->get_record_from_path(path);
  if (record.data.recordType != kHFSPlusFolderRecord)
    return;
  
  auto node = m_catalog_tree->get_folder_contents(record.data.folder.folderID);
  
  for (auto it=node.lrecs.begin(); it != node.lrecs.end(); ++it)
  {
    if (it->data.recordType == kHFSPlusFolderRecord)
    {
      cout << it->data.folder.folderID << " " << it->key.nodeName.to_s() << endl;
    }
    else if (it->data.recordType == kHFSPlusFileRecord)
    {
      cout << it->data.file.fileID << " " << it->key.nodeName.to_s() << endl;
    }
  }
}

auto HFSVolume::read(int64_t offset, size_t sz) -> ByteBuffer
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

auto HFSVolume::read_file(string const& path, string const& mp) -> bool
{
  auto r = m_catalog_tree->get_record_from_path(path);
  if (r.data.recordType != kHFSPlusFileRecord)
    return false;
  
  // TODO
  /*
  auto xattr = get_xattr(r.data.file.fileID, "com.apple.decmpfs");
  if ()
  {
    
  }
  */
  
  HFSFile f(this, r.data.file.dataFork, r.data.file.fileID);
  f.read_all_to_file(path, mp, true);
  
  return true;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
