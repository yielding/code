#include "catalog_btree.h"

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
bool compare_id(HFSCatalogNodeID first, HFSCatalogNodeID second);

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
    cout << "Catalogtree constructor\n";
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
  
  uint16_t i;
  for (i=0; i<l.nodeName.length; i++)
  {
    if (i >= r.nodeName.length) return 1;
    uint16_t cL = (l.nodeName.unicode[i] == ':') ? '/' : l.nodeName.unicode[i];
    uint16_t cR = (r.nodeName.unicode[i] == ':') ? '/' : r.nodeName.unicode[i];
    
    if (cL < cR) return -1;
    if (cL > cR) return  1;
  }
  
  if (i < r.nodeName.length)
    return -1;
  
  return 0;
}

auto CatalogTree::search_by_cnid(HFSCatalogNodeID cnid) -> CatalogLeafRecord
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
  
  return CatalogLeafRecord();
}

auto CatalogTree::get_record_from_path(string const& path) -> CatalogLeafRecord
{
  using namespace boost;
  
  CatalogLeafRecord leaf;
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

auto CatalogTree::read_index_record(ByteBuffer& buffer, uint32_t offset) const 
  -> CatalogIndexRecord
{
  CatalogIndexRecord record;
  buffer.offset(offset);
  record.key.read_from(buffer);
  record.pointer = buffer.get_uint4_be();
  
  return record;
}

auto CatalogTree::read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
  -> CatalogLeafRecord
{
  CatalogLeafRecord record;
  buffer.offset(offset);
  record.key.read_from(buffer);
  record.data.read_from(buffer);
  
  return record;
}

auto CatalogTree::get_folder_contents(HFSCatalogNodeID folderID)
  -> CatalogTreeNode
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
