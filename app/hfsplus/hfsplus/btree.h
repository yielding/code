#ifndef BTREE_H_TT9U7QXW
#define BTREE_H_TT9U7QXW

#include "hfs_file.h"

#include <boost/tuple/tuple.hpp>
#include <cassert>
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
  auto read_btree_node(uint32_t node_no) -> std::pair<int, vector<ByteBuffer>>;
  auto read_empty_space() -> ByteBuffer;

private:
  HFSTree const& self() 
  { 
    return static_cast<HFSTree const&>(*this); 
  }

private:
  BTHeaderRec m_header_record;
  BTNodeDescriptor m_last_btnode;
  ByteBuffer m_maprec;

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
  BTNodeDescriptor btnode(b0);

  if (btnode.kind != kBTHeaderNode)
    cout << "Should be THeaderNode not not die~~~~~~~~~~~\n";

  m_header_record.read_from(b0, BTNodeDescriptor::size_of());

  m_node_size       = m_header_record.nodeSize;
  m_nodes_in_block  = file->block_size() / m_node_size;
  m_blocks_for_node = m_node_size / file->block_size();
  m_last_record_no  = 0;

  vector<ByteBuffer> buffers; int type; 
  boost::tie(type, buffers) = read_btree_node(0);
  assert(buffers.size() == 2);
  m_maprec = buffers[1];
}

template <typename HFSTree>
bool BTree<HFSTree>::node_in_use(uint32_t no)
{
  auto this_byte = m_maprec[no / 8];
  return (this_byte & (1 << (7 - (no % 8)))) != 0;
}

#not tested
template <typename HFSTree>
ByteBuffer BTree<HFSTree>::read_empty_space()
{
  ByteBuffer res;
  int64_t free_nodes = 0;
  for (uint32_t i=0; i<m_header_record.totalNodes; i++)
  {
    if (!node_in_use(i))
    {
      free_nodes++;
      res.append(read_node(i));
    }
  }
  
  assert(free_nodes == m_header_record.freeNodes);
  
  return res;
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
auto BTree<HFSTree>::read_btree_node(uint32_t node_no) -> std::pair<int, vector<ByteBuffer>>
{
  m_last_record_no = node_no;
  auto node = read_node(node_no);
  BTNodeDescriptor btnode(node);
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
      // TODO 여기가 제일 어렵네...
      // child 는 UBInt32("nodeNumber"): unsigned big-endian 32bit integer
      offset = self().parse_key(node, offset);
      auto child = node.slice(offset, offset+4);
      buffers.push_back(child);
    }
    
    res_type = kBTIndexNode;
  }
  else if (btnode.kind == kBTLeafNode)
  {
    for (int i=0; i<btnode.numRecords; i++)
    {
      auto offset = offsets[btnode.numRecords-i-1];
      offset = self().parse_key(node, offset);
      auto data = self().parse_data(node, offset);
      buffers.push_back(data);
    }
    
    res_type = kBTLeafNode;
  }
  else
  {
    res_type = -10;
  }
  
  return make_pair(kBTHeaderNode, buffers);
}

template <typename HFSTree>
auto BTree<HFSTree>::search(key, uint32_t node_no=0xFFFFFFFF) -> ?
{
  auto node = (node_no == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no;
  
  int type; vector<ByteBuffer> buffers;
  boost::tie(type, buffers) = self.read_btree_node(node);
  if (type == kBTIndexNode)
  {
    
  }
  else if (type == kBTLeafNode)
  {
    
  }

}

template <typename HFSTree>
auto BTree<HFSTree>::traverse(uint32_t node_no) -> ?
{
}

template <typename HFSTree>
auto BTree<HFSTree>::search_multiple(uint32_t node_no) -> ?
{
}


////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif