#ifndef CATALOG_BTREE_H_53HYMVEK
#define CATALOG_BTREE_H_53HYMVEK

#include "btree.h"

#include <boost/algorithm/string.hpp>
#include <string>
#include <vector>

using namespace utility::hex;
using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct CatalogIndexRecord
{
  HFSPlusCatalogKey key;
  uint32_t pointer;
};

struct CatalogLeafRecord
{
  HFSPlusCatalogKey  key;
  HFSPlusCatalogData data;
};

struct CatalogTreeNode
{
  int type;
  vector<CatalogIndexRecord> irecs;
  vector<CatalogLeafRecord>  lrecs;
};

class CatalogTree;

template <> 
struct BTreeTraits<CatalogTree>
{
  typedef CatalogTreeNode   Node;
  typedef CatalogLeafRecord LeafRecord;
  typedef HFSPlusCatalogKey SearchKey;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class CatalogTree: public BTree<CatalogTree>
{
public:
  CatalogTree(HFSFile* file);
  ~CatalogTree();
  
public:
  int compare_keys(HFSPlusCatalogKey const& key1, HFSPlusCatalogKey const& key2) const
  {
    return -1;
  }

  auto read_index_record(ByteBuffer& buffer, uint32_t offset) const 
    -> CatalogIndexRecord
  {
    CatalogIndexRecord record;
    record.key.read_from(buffer);
    record.pointer = buffer.get_uint4_be();

    return record;
  }

  auto read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
    -> CatalogLeafRecord
  {
    CatalogLeafRecord record;
    record.key.read_from(buffer);
    record.data.read_from(buffer);

    return record;
  }
  
  void print_leaf(BufferPair const&)
  {
  }

protected:
  auto search_by_cnid(HFSCatalogNodeID cnid) -> CatalogLeafRecord
  {
    HFSPlusCatalogKey key;
    key.parentID = cnid;

    auto thread = search(key);
    assert(thread.data.recordType == 3 || thread.data.recordType == 4);
    
    key.parentID = thread.data.thread.parentID;
    key.nodeName = thread.data.thread.nodeName;
    
    return search(key);
  }
  
  void get_record_from_path(string const& path)
  {
    if (!boost::starts_with(path, "/"))
      return;
      // return make_pair(nullptr, nullptr);
    
    if (path == "/")
      return;
    
    search_by_cnid(kHFSRootFolderID);
    
    // TODO
    
  }
  
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
