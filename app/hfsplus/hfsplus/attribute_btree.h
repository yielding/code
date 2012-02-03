#ifndef ATTRIBUTE_BTREE_H_J838DMAI
#define ATTRIBUTE_BTREE_H_J838DMAI

#include "btree.h"

#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef BTreeRecord<HFSPlusAttrKey, HFSPlusAttrData> AttrRecord;
typedef BTreeNode<AttrRecord> AttrNode;

class AttributeTree;

template <> 
struct BTreeTraits<AttributeTree>
{
  typedef AttrNode Node;
  typedef AttrRecord Record;
  typedef HFSPlusAttrKey SearchKey;
};

class AttributeTree: public BTree<AttributeTree>
{
public:
  AttributeTree(HFSFile* file);
  ~AttributeTree();
  
public:
  int compare_keys(HFSPlusAttrKey const& key1, HFSPlusAttrKey const& key2) const;

public:
  auto get_all_attributes(HFSCatalogNodeID folderID) -> AttrNode;

  auto get_attribute(HFSCatalogNodeID cnid, std::string const& key) -> AttrRecord;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
