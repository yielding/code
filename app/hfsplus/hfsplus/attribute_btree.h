#ifndef ATTRIBUTE_BTREE_H_J838DMAI
#define ATTRIBUTE_BTREE_H_J838DMAI

#include "btree.h"

#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
// REMARK which is better (k, v) or { k; v; }
//
////////////////////////////////////////////////////////////////////////////////
struct AttributeIndexRecord
{
  HFSPlusCatalogKey key;
  uint32_t pointer;
};

struct AttributeLeafRecord
{
  AttributeLeafRecord() { m_empty = true; }

  void empty(bool v) { m_empty = v; }
  bool empty() const { 
    if (m_empty) assert(data.recordType == 0); 
    return m_empty;
  }
  
  bool m_empty;
  HFSPlusCatalogKey  key;
  HFSPlusCatalogData data;
};

struct AttributeTreeNode
{
  int type;
  vector<AttributeIndexRecord> irecs;
  vector<AttributeLeafRecord>  lrecs;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class AttributeTree;

template <> 
struct BTreeTraits<AttributeTree>
{
  typedef AttributeTreeNode   Node;
  typedef AttributeLeafRecord LeafRecord;
  typedef HFSPlusAttrKey      SearchKey;
};

class AttributeTree: public BTree<AttributeTree>
{
public:
  AttributeTree(HFSFile* file);
  ~AttributeTree();
  
public:
  int compare_keys(HFSPlusAttrKey const& key1, HFSPlusAttrKey const& key2) const;

  auto read_index_record(ByteBuffer& buffer, uint32_t offset) const 
    -> AttributeIndexRecord;

  auto read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
    -> AttributeLeafRecord;
  
public:
  auto get_all_attributes(HFSCatalogNodeID folderID) -> AttributeTreeNode;

  auto get_attribute(HFSCatalogNodeID cnid, std::string const& key) -> AttributeLeafRecord;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
