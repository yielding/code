#ifndef BTREE_H_TT9U7QXW
#define BTREE_H_TT9U7QXW

#include "hfs_file.h"

#include <iostream>

using namespace utility::hex;
using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSFile;

template <typename HFSTree>
class BTree
{
public:
  BTree(HFSFile* file);

public:
  auto node_in_use(uint32_t no) -> bool;
  auto read_node(uint32_t node_no) -> ByteBuffer;
  auto node_btree_node(uint32_t node_no) -> void;

private:
  HFSTree const& self() 
  { 
    return static_cast<HFSTree const&>(*this); 
  }

private:
  BTHeaderRec m_header_record;

  HFSFile*   m_file;
  uint32_t   m_nodes_in_block;
  uint32_t   m_blocks_for_node;
  uint32_t   m_node_size;
  uint32_t   m_last_record_no;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename HFSTree>
BTree<HFSTree>::BTree(HFSFile* file) 
  : m_file(file)
{
  auto b0 = m_file->read_block(0);
  BTNodeDescriptor btnode;
  btnode.read_from(b0);

  if (btnode.kind == kBTHeaderNode)
    cout << "Should be THeaderNode not not die~~~~~~~~~~~\n";
  // assert(btnode.kind == kBTHeaderNode);

  m_header_record.read_from(b0, BTNodeDescriptor::size_of());

  m_node_size       = m_header_record.nodeSize;
cout << "m_node_size : " << m_node_size << endl;

  m_nodes_in_block  = file->block_size() / m_node_size;
  m_blocks_for_node = m_node_size / file->block_size();
  m_last_record_no  = 0;

  // TODO here
  // auto header = ;
}

template <typename HFSTree>
ByteBuffer BTree<HFSTree>::read_node(uint32_t node_no)
{
  ByteBuffer node;

  for (auto i=0; i<m_blocks_for_node; i++)
  {
    auto b = m_file->read_block(node_no * m_blocks_for_node + i); 
    node.append(b);
  }

  return node;
}

template <typename HFSTree>
auto BTree<HFSTree>::node_btree_node(uint32_t node_no) -> void
{
  m_last_record_no = node_no;
  auto node = read_node(node_no);
}

template <typename HFSTree>
bool BTree<HFSTree>::node_in_use(uint32_t no)
{
  return false;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
