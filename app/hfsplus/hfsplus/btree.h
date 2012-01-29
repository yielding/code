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
namespace {
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
  
  auto read_empty_space() -> ByteBuffer;
  
  template <typename SearchKey>
  auto search(SearchKey const& key, uint32_t node_no=0xFFFFFFFF) 
    -> BufferPair;
  
  auto search_multiple(ByteBuffer& search_key, Callback& call) 
    -> BTreeNode;
  
  auto traverse(uint32_t node_no, Callback& call, uint32_t count=0) 
    -> uint32_t;
  
  auto traverse_leaf_nodes(Callback& call) -> uint32_t;
  
protected:
  auto read_node(uint32_t node_no) -> ByteBuffer;
  auto read_btree_node(uint32_t node_no) -> BTreeNode;
  auto read_offsets(BTNodeDescriptor const& btnode, ByteBuffer& buffer)
     -> vector<uint16_t>;

private:
  HFSTree const& self() { 
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
  uint32_t   m_last_node_no;
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
  auto b0 = read_node(0);
  BTNodeDescriptor btnode(b0);

  if (btnode.kind != kBTHeaderNode)
    throw std::runtime_error("Headernode type should be kbBTHeaderNode");
                             
  m_header_record.read_from(b0, BTNodeDescriptor::size_of());

  m_node_size       = m_header_record.nodeSize;
  m_nodes_in_block  = file->block_size() / m_node_size;
  m_blocks_for_node = m_node_size / file->block_size();
  m_last_record_no  = 0;

  auto offsets = read_offsets(btnode, b0);
  auto from = offsets[offsets.size() - 3];
  auto to   = offsets[offsets.size() - 4];
  m_maprec  = b0.slice(from, to);
}

template <typename HFSTree>
auto BTree<HFSTree>::read_offsets(BTNodeDescriptor const& btnode, ByteBuffer& buffer)
  -> vector<uint16_t>
{
  vector<uint16_t> offsets;
  
  buffer.offset(uint32_t(buffer.size() - 2*(btnode.numRecords+1)));
  for (auto i=0; i<=btnode.numRecords; i++) 
    offsets.push_back(buffer.get_uint2_be());
    
  return offsets;
}

template <typename HFSTree>
auto BTree<HFSTree>::node_in_use(uint32_t no) -> bool
{
  auto this_byte = m_maprec[no / 8];
  return (this_byte & (1 << (7 - (no % 8)))) != 0;
}

// not tested
template <typename HFSTree>
auto BTree<HFSTree>::read_empty_space() -> ByteBuffer 
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
auto BTree<HFSTree>::read_node(uint32_t node_no) -> ByteBuffer
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
  -> HFSTree::Node
{
  m_last_node_no = node_no;

  auto node_buffer = read_node(node_no);
  BTNodeDescriptor btnode(node_buffer); m_last_btnode = btnode;
  
  auto offsets = read_offsets(btnode, node_buffer);

  HFSTree::Node node;
  node.type = btnode.kind;
  auto self = self();
  for (auto i=0; i<btnode.numRecords; i++)
  {
    auto offset = offsets[btnode.numRecords-i-1];
    auto record = (node.type == kBTIndexNode) 
      ? ( self.read_index_record(node_buffer, offset), node.irecs.push_back(record) )
      : ( self.read_leaf_record(node_buffer, offset),  node.lrecs.push_back(record) );
  }

  /*
  if (btnode.kind == kBTIndexNode)
  {
    node.type = kBTIndexNode;
    for (int i=0; i<btnode.numRecords; i++)
    {
      auto offset = offsets[btnode.numRecords-i-1];
      auto record = self().read_index_record(node_buffer, offset);
      node.irecs.push_back(record); 

//      node_buffset.offset(offset); record.key = self().parse_key(node_buffer, offset);
//      node_buffset.offset(offset); record.pointer = node_buffer.get_uint4_be();
    }
  }
  else if (btnode.kind == kBTLeafNode)
  {
    node.type = kBTLeafNode;
    for (int i=0; i<btnode.numRecords; i++)
    {
      auto offset = offsets[btnode.numRecords-i-1];
      auto record = self().read_leaf_record(node_buffer, offset);
      node.lrecs.push_back(record);
    
//      record.key   = self().parse_key(node_buffer, offset);
//      record.value = self().parse_data(node_buffer, offset);
    }
  }
  else
  {
    node.type = -10;
  }
  */
  
  return node;
}

template <typename HFSTree> template <typename SearchKey>
auto BTree<HFSTree>::search(SearchKey const& search_key, uint32_t node_no_) 
  -> BufferPair
{
  auto node_no = (node_no_ == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no_;
  
  auto node = this->read_btree_node(node_no);
  if (node.type == kBTIndexNode)
  {
    for (auto i=0; i<node.data.size(); i++)
    {
      if (self().compare_keys(search_key, node.data[i].first) >= 0)
        continue;
      
      auto j = (i > 0) ? i - 1 : 0;
      return search(search_key, node.data[j]);
    }
    
    return search(search_key, node.data[node.data.size()-1]);
  }

  if (node.type == kBTLeafNode)
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
  auto& buffer = node.data;
  if (node.type == kBTIndexNode)
  {
    for (auto i=0; i<buffer.size(); i++)
      count += traverse(buffer[i], call);
  }
  else if (node.type == kBTLeafNode)
  {
    for (auto it=buffer.begin(); it != buffer.end(); ++it)
    {
      call.empty() ? self().print_leaf(*it) : call(*it);
      count++;
    }
  }
  
  return count;
}

template <typename HFSTree>
auto BTree<HFSTree>::traverse_leaf_nodes(Callback& call) -> uint32_t
{
  auto node_no = m_header_record.firstLeafNode;
  uint32_t count = 0;
  while (node_no != 0)
  {
    auto node = read_btree_node(node_no);
    count += node.data.size();
    
    for (auto it=node.data.begin(); it != node.data.end(); ++it)
      call.empty() ? self().print_leaf() : call(*it);
    
    node_no = m_last_btnode.fLink;
  }
  
  return count;
}

template <typename HFSTree>
auto BTree<HFSTree>::search_multiple(ByteBuffer& search_key, Callback& call) -> BTreeNode
{
  search(search_key);
  auto node_no = m_last_node_no;
  auto record_no = m_last_record_no;
  BTreeNode node;
  while (node_no != 0)
  {
    auto node_ = read_btree_node(node_no);
    for (auto it=node_.data.begin(); it!=node_.data.end(); ++it)
    {
      if (call(*it))
        node.data.push_back(*it);
      else
        return node;
    }
    
    node_no = m_last_btnode.fLink;
    record_no = 0;
  }
  
  return node;
}


////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
