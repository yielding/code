#ifndef BTREE_H_TT9U7QXW
#define BTREE_H_TT9U7QXW

#include "hfs_file.h"

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
class HFSFile;

typedef boost::function<bool(HFSCatalogNodeID)> Callback;

template <typename HFSTree> 
struct BTreeTraits;

template <typename KeyT, typename ValueT>
struct BTreeRecord
{
  BTreeRecord(int t_=kBEmpty) { m_empty = (t_ == kBEmpty); }
  
  bool empty() const 
  { 
    return m_empty; 
  }
  
  bool operator<(BTreeRecord const& rhs) const
  {
    return key < rhs.key;
  }
  
  bool operator==(BTreeRecord const& rhs) const
  {
    return key == rhs.key;
  }

  KeyT     key;
  ValueT   data;
  uint32_t pointer;
  
private:
  bool m_empty;
};

template <typename HFSRecord>
struct BTreeNode
{
  BTreeNode(int t_=kBEmpty) :type(t_) {}
  
  bool empty() const { return type == kBEmpty;      }
  bool leaf()  const { return type == kBTLeafNode ; }
  bool index() const { return type == kBTIndexNode; }
  
  void push_back(HFSRecord& rec) { recs.push_back(rec); }
  
  vector<HFSRecord> recs;
  
private:
  int type;
};

template <typename HFSTree>
class BTree
{
public:
  typedef typename BTreeTraits<HFSTree>::Node Node;
  typedef typename BTreeTraits<HFSTree>::Record Record;
  typedef typename BTreeTraits<HFSTree>::SearchKey SearchKey;

  typedef boost::function<void(ByteBuffer&)> Callback1;

  typedef boost::function<bool(Record)> Callback2;
  
public:
  BTree(HFSFile* file);

public:
  auto node_in_use(uint32_t no) -> bool;
  
  auto read_empty_space() 
    -> ByteBuffer;
  
  auto search(SearchKey const& key, uint32_t node_no=0xFFFFFFFF) 
    -> Record;
  
  auto search_multiple(SearchKey const& key, Callback const& call) 
    -> Node;
  
  auto traverse(uint32_t node_no, Callback& call, uint32_t count=0) 
    -> uint32_t;
  
  auto traverse_leaf_nodes(Callback2 call) -> uint32_t;

  auto node_size() -> uint16_t { return m_header_record.nodeSize; }

  // auto traverse_leaf_slacks(Callback1 call) -> void;

protected:
  auto read_node(uint32_t node_no) -> ByteBuffer;
  auto read_btree_node(uint32_t node_no) -> Node;
  auto read_btree_unused_rec(uint32_t node_no) -> ByteBuffer;
  auto read_offsets(BTNodeDescriptor const& btnode, ByteBuffer& buffer)
    -> vector<uint16_t>;

  template <typename RecordT>
  auto read_index_record(ByteBuffer& buffer, uint32_t offset) const 
    -> RecordT;
  
  template <typename RecordT>
  auto read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
    -> RecordT;

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

//
// all empty space is clear with zero
//
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
      auto x =read_node(i);
      res.append( x );
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
    ByteBuffer b = m_file->read_block(node_no * m_blocks_for_node + i); 
    node.append(b);
  }

  return node;
}

template <typename HFSTree>
auto BTree<HFSTree>::read_btree_node(uint32_t node_no) 
  -> Node
{
  m_last_node_no = node_no;

  auto node_buffer = read_node(node_no);
  BTNodeDescriptor btnode(node_buffer); m_last_btnode = btnode;
  
  auto offsets = read_offsets(btnode, node_buffer);

  Node node(btnode.kind);
  for (auto i=0; i<btnode.numRecords; i++)
  {
    auto offset = offsets[btnode.numRecords-i];
    if (node.index())
    {
      auto record = read_index_record<Record>(node_buffer, offset);
      node.push_back(record);
    }
    else if (node.leaf())
    {
      auto record = read_leaf_record<Record>(node_buffer, offset);
      node.recs.push_back(record);
    }
    else
    {
      assert(0);
    }
  }

  return node;
}

template <typename HFSTree>
auto BTree<HFSTree>::read_btree_unused_rec(uint32_t node_no) 
  -> ByteBuffer
{
  auto node_buffer = read_node(node_no);
  BTNodeDescriptor btnode(node_buffer); m_last_btnode = btnode;

  auto offsets = read_offsets(btnode, node_buffer);

  auto from = offsets[0];
  auto to   = node_buffer.size() - offsets.size() * 2;
  return node_buffer.slice(from, to);
}

template <typename HFSTree> template <typename RecordT>
auto BTree<HFSTree>::read_index_record(ByteBuffer& buffer, uint32_t offset) const 
  -> RecordT 
{
  buffer.offset(offset);
  
  RecordT record(kBTIndexNode);
  record.key.read_from(buffer);
  record.pointer = buffer.get_uint4_be();
  
  return record;
}

template <typename HFSTree> template <typename RecordT>
auto BTree<HFSTree>::read_leaf_record(ByteBuffer& buffer, uint32_t offset) const 
  -> RecordT
{
  buffer.offset(offset);
  
  RecordT record(kBTLeafNode);
  record.key.read_from(buffer);
  record.data.read_from(buffer);
  
  return record;
}

template <typename HFSTree> 
auto BTree<HFSTree>::search(SearchKey const& search_key, uint32_t node_no_) 
  -> Record
{
  auto node_no = (node_no_ == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no_;
  
  auto node = this->read_btree_node(node_no);
  if (node.index())
  {
    for (auto i=0; i<node.recs.size(); i++)
    {
      if (self().compare_keys(search_key, node.recs[i].key) < 0)
      {
        auto index = (i > 0) ? i - 1 : i;
        return search(search_key, node.recs[index].pointer);
      }
    }
    
    return search(search_key, node.recs[node.recs.size()-1].pointer);
  }

  if (node.leaf())
  {
    m_last_record_no = 0;
    for (auto it=node.recs.begin(); it != node.recs.end(); ++it)
    {
      auto result = self().compare_keys(search_key, it->key);
      if (result == 0)
        return *it;
      
      if (result < 0)
        break;
      
      m_last_record_no++;
    }
  }

  return Record();
}

template <typename HFSTree>
auto BTree<HFSTree>::search_multiple(SearchKey const& key, Callback const& call) 
  -> Node
{
  // 아래의 search에 의해 leaf에 대한 위치가 caching 될 것이라 사료됨.
  search(key);
  
  auto node_no = m_last_node_no;
  auto record_no = m_last_record_no;
  Node node;
  
  while (node_no != 0)
  {
    auto node_ = read_btree_node(node_no);
    if (node_.index())
      return node;
    
    auto it = node_.recs.begin();
    advance(it, record_no);
    for (; it!=node_.recs.end(); ++it)
    {
      if (call(it->key.parentID))
        node.recs.push_back(*it);
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
  if (call.empty())
    return 0;
  
  auto node_no = (node_no_ == 0xFFFFFFFF)
    ? m_header_record.rootNode
    : node_no_;

  auto node = this->read_btree_node(node_no);
  if (node.index())
  {
    for (auto i=0; i<node.recs.size(); i++)
      count += traverse(node.recs[i], call);
  }
  else if (node.leaf())
  {
    for (auto it=node.recs.begin(); it != node.recs.end(); ++it)
    {
      call(*it);
      count++;
    }
  }
  
  return count;
}

/*
template <typename HFSTree>
auto BTree<HFSTree>::traverse_leaf_slacks(Callback1 call) -> void
{
  if (call.empty())
    return;
  
  auto node_no = m_header_record.firstLeafNode;
  while (node_no != 0)
  {
    auto buffer = read_btree_unused_rec(node_no);
    // cout << node_no << endl;
    call(buffer);
    node_no = m_last_btnode.fLink;
  }
}
*/

template <typename HFSTree>
auto BTree<HFSTree>::traverse_leaf_nodes(Callback2 call) -> uint32_t
{
  if (call.empty())
    return 0;
  
  uint32_t count = 0;
  auto node_no = m_header_record.firstLeafNode;
  while (node_no != 0)
  {
    auto node = this->read_btree_node(node_no);
    if (node.index())
      continue;

    count += node.recs.size();
    for (auto it=node.recs.begin(); it != node.recs.end(); ++it)
      call(*it);
      
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
