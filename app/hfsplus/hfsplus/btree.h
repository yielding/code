#ifndef BTREE_H_TT9U7QXW
#define BTREE_H_TT9U7QXW

#include "hfs_file.h"

#include <boost/tuple/tuple.hpp>
#include <boost/function.hpp>
#include <cassert>
#include <iostream>

using namespace utility::hex;
using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace  {
  typedef pair<ByteBuffer, ByteBuffer> BufferPair;
  struct BTreeNode {
    int type;
    vector<BufferPair> data;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSFile;

typedef boost::function<void(BufferPair const&)> Callback;

template <typename HFSTree>
class BTree
{
public:
  BTree(HFSFile* file);

public:
  auto node_in_use(uint32_t no) -> bool;
  auto read_node(uint32_t node_no) -> ByteBuffer;
  auto read_btree_node(uint32_t node_no) -> BTreeNode;
  auto read_empty_space() -> ByteBuffer;

  auto search(ByteBuffer key, uint32_t node_no=0xFFFFFFFF) -> BufferPair;
  auto traverse(uint32_t node_no, Callback& call, uint32_t count=0) -> uint32_t;

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

  auto root_node = read_btree_node(0);
  assert(root_node.data.size() == 2);
  m_maprec = root_node.data[1].first;
}

template <typename HFSTree>
bool BTree<HFSTree>::node_in_use(uint32_t no)
{
  auto this_byte = m_maprec[no / 8];
  return (this_byte & (1 << (7 - (no % 8)))) != 0;
}

// not tested
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
auto BTree<HFSTree>::read_btree_node(uint32_t node_no) -> BTreeNode
{
  m_last_record_no = node_no;
  auto node_buffer = read_node(node_no);
  BTNodeDescriptor btnode(node_buffer);
  m_last_btnode = btnode;
  
  // REMARK
  // read one more offset to find out the end of macrec.
  node_buffer.offset(uint32_t(node_buffer.size() - 2*(btnode.numRecords+1)));
  vector<uint16_t> offsets;
  for (auto i=0; i<=btnode.numRecords; i++) 
    offsets.push_back(node_buffer.get_uint2_be());
    
  BTreeNode node;
  if (btnode.kind == kBTHeaderNode)
  {
    uint32_t start = BTNodeDescriptor::size_of();
    auto header = node_buffer.slice(start, start + BTHeaderRec::size_of());
    node.data.push_back(make_pair(header, ByteBuffer()));
    
    auto from = offsets[offsets.size()-3];
    auto to   = offsets[offsets.size()-4];
    auto maprec = node_buffer.slice(from, to);
    node.data.push_back(make_pair(maprec, ByteBuffer()));
    node.type = kBTHeaderNode;
  }
  else if (btnode.kind == kBTIndexNode)
  {
    for (int i=0; i<btnode.numRecords; i++)
    {
      auto offset = offsets[btnode.numRecords-i-1];
      // TODO 여기가 제일 어렵네...
      // child 는 UBInt32("nodeNumber"): unsigned big-endian 32bit integer
      auto child = node_buffer.slice(offset, offset+4);
      node.data.push_back(make_pair(child, ByteBuffer()));
    }
    
    node.type = kBTIndexNode;
  }
  else if (btnode.kind == kBTLeafNode)
  {
    for (int i=0; i<btnode.numRecords; i++)
    {
      auto offset = offsets[btnode.numRecords-i-1];
      auto key   = self().parse_key(node_buffer, offset);
      auto value = self().parse_data(node_buffer, offset);
      node.data.push_back(make_pair(key, value));
    }
    
    node.type = kBTLeafNode; 
  }
  else
  {
    node.type = -10;
  }
  
  return node;
}

template <typename HFSTree>
auto BTree<HFSTree>::search(ByteBuffer search_key, uint32_t node_no_) -> BufferPair
{
  auto node_no = (node_no_ == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no_;
  
  auto node = this->read_btree_node(node_no);
  if (node.type == kBTIndexNode)
  {
    for (auto i=0; i<node.data.size(); i++)
    {
      if (self().compare_keys(search_key, node.data[i].first) < 0)
      {
        if (i > 0) i--;
        return search(search_key, node.data[i]);
      }
    }
    
    return search(search_key, node.data[node.data.size()-1]);
  }
  else if (node.type == kBTLeafNode)
  {
    m_last_record_no = 0;
    for (auto it=node.data.begin(); it != node.data.end(); ++it)
    {
      auto key    = it->first;
      auto value  = it->second;
      auto result = self().compareKey(search_key, key);
      if (result == 0)
        return *it;
      
      if (result < 0)
        break;
      
      m_last_record_no++;
    }
  }

  return make_pair(ByteBuffer(), ByteBuffer());
}

template <typename HFSTree>
auto BTree<HFSTree>::traverse(uint32_t node_no_, Callback& call, uint32_t count) 
  -> uint32_t
{
  auto node_no = (node_no_ == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no_;

  auto node = this->read_btree_node(node_no);
  if (node.type == kBTIndexNode)
  {
    
  }
  else if (node.type == kBTLeafNode)
  {
    
  }
  
  return count;
}

//template <typename HFSTree>
//auto BTree<HFSTree>::search_multiple(uint32_t node_no) -> ?
//{
//}


////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif