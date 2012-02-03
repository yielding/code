#include "attribute_btree.h"

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

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
AttributeTree::AttributeTree(HFSFile* file)
  : BTree<AttributeTree>(file)
{
}

AttributeTree::~AttributeTree()
{
}

int AttributeTree::compare_keys(HFSPlusAttrKey const& l, HFSPlusAttrKey const& r) const
{
  if (l.fileID < r.fileID)
    return -1;

  if (l.fileID > r.fileID)
    return 1;
  
  uint16_t i;
  for (i=0; i<l.name.length; i++)
  {
    if(i >= r.name.length) 
      return 1;
    
    uint16_t cl = l.name.unicode[i];
    uint16_t cr = r.name.unicode[i];
      
    if (cl < cr) return -1;
    if (cl > cr) return  1;
  }
  
  if (i < r.name.length) 
    return -1;
  
  return 0;
}

// TODO REFACTOR to btree with template
auto AttributeTree::read_index_record(ByteBuffer& buffer, uint32_t offset) const 
  -> AttributeIndexRecord
{
  AttributeIndexRecord record;
  buffer.offset(offset);
  record.key.read_from(buffer);
  record.pointer = buffer.get_uint4_be();
  
  return record;
}

auto AttributeTree::read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
  -> AttributeLeafRecord
{
  AttributeLeafRecord record;
  buffer.offset(offset);
  record.key.read_from(buffer);
  record.data.read_from(buffer);
  
  return record;
}

auto AttributeTree::get_all_attributes(HFSCatalogNodeID folderID) -> AttributeTreeNode
{
  AttributeTreeNode node;
  return node;
}

auto AttributeTree::get_attribute(HFSCatalogNodeID cnid, string const& key) -> AttributeLeafRecord
{
  AttributeLeafRecord record;
  
  return record;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
