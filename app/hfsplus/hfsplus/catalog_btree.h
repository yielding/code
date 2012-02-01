#ifndef CATALOG_BTREE_H_53HYMVEK
#define CATALOG_BTREE_H_53HYMVEK

#include "btree.h"

#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// REMARK which is better (k, v) or { k; v }
//
////////////////////////////////////////////////////////////////////////////////
struct CatalogIndexRecord
{
  HFSPlusCatalogKey key;
  uint32_t pointer;
};

struct CatalogLeafRecord
{
  CatalogLeafRecord() { m_empty = true; }

  void empty(bool v) { m_empty = v; }
  bool empty() const { 
    if (m_empty) assert(data.recordType == 0); 
    return m_empty;
  }
  
  bool m_empty;
  HFSPlusCatalogKey  key;
  HFSPlusCatalogData data;
};

struct CatalogTreeNode
{
  int type;
  vector<CatalogIndexRecord> irecs;
  vector<CatalogLeafRecord>  lrecs;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class CatalogTree;

template <> 
struct BTreeTraits<CatalogTree>
{
  typedef CatalogTreeNode   Node;
  typedef CatalogLeafRecord LeafRecord;
  typedef HFSPlusCatalogKey SearchKey;
};

class CatalogTree: public BTree<CatalogTree>
{
public:
  CatalogTree(HFSFile* file);
  ~CatalogTree();
  
public:
  int compare_keys(HFSPlusCatalogKey const& key1, HFSPlusCatalogKey const& key2) const;

  auto read_index_record(ByteBuffer& buffer, uint32_t offset) const 
    -> CatalogIndexRecord;

  auto read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
    -> CatalogLeafRecord;
  
  void print_leaf(BufferPair const&);

public:
  auto get_record_from_path(string const& path) -> CatalogLeafRecord;
  
  auto get_folder_contents(HFSCatalogNodeID folderID) -> CatalogTreeNode;

protected:
  auto search_by_cnid(HFSCatalogNodeID cnid) -> CatalogLeafRecord;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
