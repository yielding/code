#include "catalog_btree.h"
#include "unicode_compare.h"

#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>
#include <iostream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  const uint16_t METADATA_DIR[] = 
  {
    0, 0, 0, 0, 
    'H', 'F', 'S', '+', ' ', 
    'P', 'r', 'i', 'v', 'a', 't', 'e', ' ', 'D', 'a', 't', 'a'
  };

  bool compare_id(HFSCatalogNodeID first, HFSCatalogNodeID second)
  {
    return first == second;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CatalogTree::CatalogTree(HFSFile* file)
  : BTree<CatalogTree>(file)
{
}

CatalogTree::~CatalogTree()
{
}

void CatalogTree::print_leaf(BufferPair const&)
{
}

int CatalogTree::compare_keys(HFSPlusCatalogKey const& l, HFSPlusCatalogKey const& r) const
{
  if (l.parentID < r.parentID)
    return -1;

  if (l.parentID > r.parentID)
    return 1;

  return FastUnicodeCompare(l.nodeName.unicode, l.nodeName.length, 
                            r.nodeName.unicode, r.nodeName.length);
}

auto CatalogTree::search_by_cnid(HFSCatalogNodeID cnid) -> CatalogRecord
{
  HFSPlusCatalogKey key;
  key.parentID = cnid;
  
  auto thread = search(key);
  if (thread.data.recordType == 3 || thread.data.recordType == 4)
  {
    key.parentID = thread.data.thread.parentID;
    key.nodeName = thread.data.thread.nodeName;
    
    return search(key);
  }
  
  return CatalogRecord();
}

auto CatalogTree::get_record_from_path(string const& path) -> CatalogRecord
{
  using namespace boost;
  
  CatalogRecord leaf;
  if (!boost::starts_with(path, "/"))
    return leaf;
  
  if (path == "/")
    return search_by_cnid(kHFSRootFolderID);
    
  HFSCatalogNodeID parentID = kHFSRootFolderID;
    
  // REMAKR: pc[0] is always empty
  vector<string> pc; split(pc, path, is_any_of("/"));
  
  for (auto i=1; i<pc.size(); i++)
  {
    if (pc[i].empty()) break;
    
    // TODO REFACTOR
    HFSPlusCatalogKey key;
    key.parentID = parentID;
    key.nodeName.from_ascii(pc[i]);
    key.keyLength = 6 + 2*pc[i].size();
    leaf = search(key);
    if (leaf.empty())
      return leaf;
    
    if (leaf.data.recordType == kHFSPlusFolderRecord)
      parentID = leaf.data.folder.folderID;
    else
      break;
  }
  
  return leaf;
}

/*
auto CatalogTree::read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
  -> CatalogRecord
{
  CatalogRecord record(kBTLeafNode);
  
  buffer.offset(offset);
  record.key.read_from(buffer);
  record.data.read_from(buffer);
  
  return record;
}
*/

auto CatalogTree::get_folder_contents(HFSCatalogNodeID folderID)
  -> CatalogNode
{
  using namespace boost;

  HFSPlusCatalogKey key;
  key.parentID = folderID;
  
#ifdef WIN32
  auto f = [folderID](HFSCatalogNodeID id) -> bool {
    return key.parentID == folderID;
  };
#else
  auto f = bind(&compare_id, _1, folderID);
#endif
  
  return search_multiple(key, f);
}

auto CatalogTree::metadata_dir_id() -> HFSCatalogNodeID
{
  HFSPlusCatalogKey key;
  key.nodeName.length = sizeof(METADATA_DIR) / sizeof(uint16_t);
  key.keyLength = sizeof(key.parentID) + sizeof(key.nodeName.length) + sizeof(METADATA_DIR);
  key.parentID  = kHFSRootFolderID;
  memcpy(key.nodeName.unicode, METADATA_DIR, sizeof(METADATA_DIR));
  
  auto record = search(key);
  return record.empty() 
      ? -1 
      : record.data.folder.folderID;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
