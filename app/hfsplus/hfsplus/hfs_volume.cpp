#include "hfs_volume.h"
#include "hfs_file.h"
#include "extents_btree.h"
#include "catalog_btree.h"
#include "attribute_btree.h"

#include <cassert>
#include <stdexcept>
#include <iomanip>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
HFSVolume::HFSVolume()
  : m_opened(false)
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

bool HFSVolume::open(string const& filename)
{
  m_stream.open(filename.c_str(), ios_base::in | ios_base::out | ios_base::binary);
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

  m_allocation_file = new HFSFile(this, m_header.allocationFile, kHFSAllocationFileID);
  m_allocatioin_bitmap = m_allocation_file->read_all_to_buffer();
  m_extents_file = new HFSFile(this, m_header.extentsFile, kHFSExtentsFileID);
  m_extents_tree = new ExtentsTree(m_extents_file);
  m_catalog_file = new HFSFile(this, m_header.catalogFile, kHFSCatalogFileID);
  m_catalog_tree = new CatalogTree(m_catalog_file);
  m_attribute_file = new HFSFile(this, m_header.attributesFile, kHFSAttributesFileID);
  m_attribute_tree = new AttributeTree(m_attribute_file);

  // TODO journal

  m_opened = true;

  return true;
}

auto HFSVolume::id() -> int64_t
{
  ByteBuffer b((uint8_t*)&m_header.finderInfo[6], 8);
  return b.get_int8_be();
}

auto HFSVolume::block_in_use(uint32_t block_no) -> bool
{
  auto& buffer = m_allocatioin_bitmap.get_buffer();
  auto index = block_no / 8;
  if (index >= buffer.size())
    return false;

  uint8_t this_byte = buffer[index];

  return (this_byte & (1 << (7 - (block_no % 8)))) != 0;
}

auto HFSVolume::get_extents_overflow_for_file(HFSPlusExtentKey const& key)
-> ExtentsRecord
{
  return m_extents_tree->search_extents(key);
}

void HFSVolume::list_folder_contents(string const& path)
{
  auto record = m_catalog_tree->get_record_from_path(path);
  if (record.data.recordType != kHFSPlusFolderRecord)
    return;
  
  auto node = m_catalog_tree->get_folder_contents(record.data.folder.folderID);
  
  for (auto it=node.recs.begin(); it != node.recs.end(); ++it)
  {
    if (it->data.recordType == kHFSPlusFolderRecord)
    {
      cout << right << setw(5) << it->data.folder.folderID 
           << " "   << it->key.nodeName.to_s() << endl;
    }
    else if (it->data.recordType == kHFSPlusFileRecord)
    {
      cout << right << setw(5) << it->data.file.fileID 
           << " "   << it->key.nodeName.to_s() << endl;
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
  
  // TODO for compressed image
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

auto HFSVolume::read_journal() -> ByteBuffer
{
  int64_t beg = m_header.journalInfoBlock * m_block_size;
  auto b0 = read(beg, m_block_size);
  JournalInfoBlock jib(b0);
  m_journal_offset = jib.offset;
  m_journal_size   = jib.size;
  
  auto journal = read(jib.offset, size_t(jib.size));
  journal_header jh(journal); 
  m_sector_size = jh.jhdr_size;

  journal.flip();
  return journal;
}

auto HFSVolume::write(int64_t offset, utility::hex::ByteBuffer& b) 
 -> void
{
  m_stream.seekp(offset, ios_base::beg);
  if (m_stream.fail())
    throw std::runtime_error("seekp for write error");
  
  // see the test code: ByteBuffer has an operator uint8_t*
  uint8_t* buffer = b;
  m_stream.write((char*)buffer, b.size());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
