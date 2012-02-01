#ifndef EXTENTS_BTREE_H_SNJZPJIW
#define EXTENTS_BTREE_H_SNJZPJIW

#include "btree.h"

#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct ExtentsIndexRecord
{
  HFSPlusExtentKey key;
  uint32_t pointer;
};

struct ExtentsLeafRecord
{
  ExtentsLeafRecord() { m_empty = true; }
  void empty(bool v)  { m_empty = v;    }
  bool empty() const  { return m_empty; }
  
  bool m_empty; 
  HFSPlusExtentKey    key;
  HFSPlusExtentRecord data;
};

struct ExtentsTreeNode
{
  int type;
  vector<ExtentsIndexRecord> irecs;
  vector<ExtentsLeafRecord>  lrecs;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class ExtentsTree;

template <> 
struct BTreeTraits<ExtentsTree>
{
  typedef ExtentsTreeNode   Node;
  typedef ExtentsLeafRecord LeafRecord;
  typedef HFSPlusExtentKey  SearchKey;
};

class ExtentsTree: public BTree<ExtentsTree>
{
public:
  ExtentsTree(HFSFile* file);
  ~ExtentsTree();
  
public:
  int compare_keys(HFSPlusExtentKey const& key1, HFSPlusExtentKey const& key2) const;

  auto read_index_record(ByteBuffer& buffer, uint32_t offset) const 
    -> ExtentsIndexRecord;

  auto read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
    -> ExtentsLeafRecord;

  auto search_extents(HFSPlusExtentKey const& key) -> ExtentsLeafRecord;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
