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

auto AttributeTree::get_all_attributes(HFSCatalogNodeID folderID) -> AttrNode
{
  AttrNode node;
  return node;
}

auto AttributeTree::get_attribute(HFSCatalogNodeID cnid, string const& key) -> AttrRecord
{
  AttrRecord record;
  
  return record;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
