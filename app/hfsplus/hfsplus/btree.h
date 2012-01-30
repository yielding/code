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
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class HFSFile;

typedef boost::function<bool(HFSCatalogNodeID)> Callback;

template <typename HFSTree> 
struct BTreeTraits;

template <typename HFSTree>
class BTree
{
public:
  typedef typename BTreeTraits<HFSTree>::Node BTreeNode;
  typedef typename BTreeTraits<HFSTree>::LeafRecord LeafRecord;
  typedef typename BTreeTraits<HFSTree>::SearchKey SearchKey;
  
public:
  BTree(HFSFile* file);

public:
  auto node_in_use(uint32_t no) -> bool;
  
  auto read_empty_space() 
    -> ByteBuffer;
  
  auto search(SearchKey const& key, uint32_t node_no=0xFFFFFFFF) 
    -> LeafRecord;
  
  auto search_multiple(SearchKey const& key, Callback const& call) 
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

  HFSFile* m_file;
  uint32_t m_nodes_in_block;
  uint32_t m_blocks_for_node;
  uint32_t m_node_size;
  uint32_t m_last_record_no;
  uint32_t m_last_node_no;
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
    throw std::runtime_error("Headernode type should be kbBTHeaderNode");
                             
  m_header_record.read_from(b0);

  m_node_size       = m_header_record.nodeSize;
  m_nodes_in_block  = file->block_size() / m_node_size;
  m_blocks_for_node = m_node_size / file->block_size();
  m_last_record_no  = 0;
  
  for (int i=1; i<m_blocks_for_node; i++)
  {
    auto b1 = m_file->read_block(i);
    b0.append(b1);
  }

//  auto b1 = m_file->read_block(<#uint32_t no#>)
//  b0.append
//  b0.append(m_file->read_block(1));
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
  
  uint32_t off = uint32_t(buffer.size() - 2*(btnode.numRecords+1)); 
  buffer.offset(off);
  for (auto i=0; i<=btnode.numRecords; i++) 
    offsets.push_back(buffer.get_uint2_be());
    
  return offsets;
}

template <typename HFSTree>
auto BTree<HFSTree>::node_in_use(uint32_t no) -> bool
{
  auto buffer = m_maprec.get_buffer();
  auto this_byte = buffer[no / 8];
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
  -> BTreeNode
{
  m_last_node_no = node_no;

  auto node_buffer = read_node(node_no);
  BTNodeDescriptor btnode(node_buffer); m_last_btnode = btnode;
  
  auto offsets = read_offsets(btnode, node_buffer);

  BTreeNode node;
  node.type = btnode.kind;
  for (auto i=0; i<btnode.numRecords; i++)
  {
    auto offset = offsets[btnode.numRecords-i];
    if (node.type == kBTIndexNode)
    {
      auto record = self().read_index_record(node_buffer, offset);
      node.irecs.push_back(record);
    }
    else if (node.type == kBTLeafNode)
    {
      auto record = self().read_leaf_record(node_buffer, offset);
      node.lrecs.push_back(record);
    }
    else
    {
      assert(0);
    }
  }

  return node;
}

template <typename HFSTree> 
auto BTree<HFSTree>::search(SearchKey const& search_key, uint32_t node_no_) 
  -> LeafRecord
{
  auto node_no = (node_no_ == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no_;
  
  auto node = this->read_btree_node(node_no);
  if (node.type == kBTIndexNode)
  {
    for (auto i=0; i<node.irecs.size(); i++)
    {
      if (self().compare_keys(search_key, node.irecs[i].key) >= 0)
        continue;
      
      auto j = (i > 0) ? i - 1 : 0;
      return search(search_key, node.irecs[j].pointer);
    }
    
    return search(search_key, node.irecs[node.irecs.size()-1].pointer);
  }

  if (node.type == kBTLeafNode)
  {
    m_last_record_no = 0;
    for (auto it=node.lrecs.begin(); it != node.lrecs.end(); ++it)
    {
      auto result = self().compare_keys(search_key, it->key);
      if (result == 0)
        return *it;
      
      if (result < 0)
        break;
      
      m_last_record_no++;
    }
  }

  return LeafRecord();
}

template <typename HFSTree>
auto BTree<HFSTree>::search_multiple(SearchKey const& key, Callback const& call) 
  -> BTreeNode
{
  // 아래의 search에 의해 leaf에 대한 위치가 caching 될 것이라 사료됨.
  search(key);
  
  auto node_no = m_last_node_no;
  auto record_no = m_last_record_no;
  BTreeNode node;
  
  while (node_no != 0)
  {
    auto node_ = read_btree_node(node_no);
    if (node_.type == kBTIndexNode)
      return node;
    
    auto it = node_.lrecs.begin();
    advance(it, record_no);
    for (; it!=node_.lrecs.end(); ++it)
    {
      if (call(it->key.parentID))
        node.lrecs.push_back(*it);
      else
        return node;
    }
    
    node_no = m_last_btnode.fLink;
    record_no = 0;
  }
  
  return node;
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
