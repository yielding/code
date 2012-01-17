#ifndef BTREE_H_TT9U7QXW
#define BTREE_H_TT9U7QXW

#include "hfs_file.h"

#include <iostream>
#include <boost/tuple/tuple.hpp>

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
  auto read_btree_node(uint32_t node_no) -> std::pair<int, vector<ByteBuffer>>;

private:
  HFSTree const& self() 
  { 
    return static_cast<HFSTree const&>(*this); 
  }

private:
  BTHeaderRec m_header_record;
  BTNodeDescriptor m_last_btnode;

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

  if (btnode.kind != kBTHeaderNode)
    cout << "Should be THeaderNode not not die~~~~~~~~~~~\n";

  
  m_header_record.read_from(b0, BTNodeDescriptor::size_of());

  m_node_size       = m_header_record.nodeSize;

  m_nodes_in_block  = file->block_size() / m_node_size;
  m_blocks_for_node = m_node_size / file->block_size();
  m_last_record_no  = 0;

  
  // TODO here
  // auto header = ;
  
  int type;
  vector<ByteBuffer> buffers;
  boost::tie(type, buffers) = read_btree_node(0);
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
auto BTree<HFSTree>::read_btree_node(uint32_t node_no) 
  -> std::pair<int, vector<ByteBuffer>>
{
  m_last_record_no = node_no;
  auto node = read_node(node_no);
  BTNodeDescriptor btnode;
  btnode.read_from(node);
  m_last_btnode = btnode;
  
  // REMARK
  // read one more offset to find out the end of macrec.
  node.offset(uint32_t(node.size() - 2*(btnode.numRecords+1)));
  vector<uint16_t> offsets;
  for (auto i=0; i<=btnode.numRecords; i++) offsets.push_back(node.get_uint2_be());
    
  vector<ByteBuffer> buffers;
  int res_type;
  if (btnode.kind == kBTHeaderNode)
  {
    /**
    // BTHeaderRec hdr; hdr.read_from(node, BTNodeDescriptor::size_of());
    **/
    
    uint32_t start = BTNodeDescriptor::size_of();
    auto header = node.slice(start, start + BTHeaderRec::size_of());
    buffers.push_back(header);
    
    auto from = offsets[offsets.size()-3];
    auto to   = offsets[offsets.size()-4];
    auto maprec = node.slice(from, to);
    buffers.push_back(maprec);
    res_type = kBTHeaderNode;
  }
  else if (btnode.kind == kBTIndexNode)
  {
    for (int i=0; i<btnode.numRecords; i++)
    {
      auto offset = offsets[btnode.numRecords-i-1];
      
      
    }
    
    res_type = kBTIndexNode;
  }
  else if (btnode.kind == kBTLeafNode)
  {
    res_type = kBTLeafNode;
  }
  else
  {
    res_type = -10;
  }
  
  return make_pair(kBTHeaderNode, buffers);
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
