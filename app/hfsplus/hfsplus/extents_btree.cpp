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

  return (l.keyLength < r.keyLength) ? -1 : 1;
}

// TODO
auto ExtentsTree::search_extents(HFSPlusExtentKey const& key)
  -> ExtentsRecord
{
  return search(key);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
