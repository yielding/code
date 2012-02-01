#include "extents_btree.h"
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
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
ExtentsTree::ExtentsTree(HFSFile* file)
  : BTree<ExtentsTree>(file)
{
}

ExtentsTree::~ExtentsTree()
{
}

int ExtentsTree::compare_keys(HFSPlusExtentKey const& l, HFSPlusExtentKey const& r) const
{
  if (l.fileID < r.fileID) return -1;
  if (l.fileID > r.fileID) return  1;

  if (l.startBlock < r.startBlock) return -1;
  if (l.startBlock > r.startBlock) return  1;

  if (l.keyLength == r.keyLength) return 0;

  if (l.keyLength < r.keyLength) return -1;
  if (l.keyLength > r.keyLength) return  1;

  return 0;
}

auto ExtentsTree::read_index_record(ByteBuffer& buffer, uint32_t offset) const 
  -> ExtentsIndexRecord
{
  ExtentsIndexRecord record;
  buffer.offset(offset);
  record.key.read_from(buffer);
  record.pointer = buffer.get_uint4_be();

  return record;
}

auto ExtentsTree::read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
  -> ExtentsLeafRecord
{
  buffer.offset(offset);
  
  ExtentsLeafRecord record;
  record.key.read_from(buffer);
  for (int i=0; i<8; i++) 
    record.data[i].read_from(buffer);
    
  return record;
}

auto ExtentsTree::search_extents(HFSPlusExtentKey const& key)
  -> ExtentsLeafRecord
{
  return search(key);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
