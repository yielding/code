#ifndef CATALOG_BTREE_H_53HYMVEK
#define CATALOG_BTREE_H_53HYMVEK

#include "btree.h"

#include <boost/algorithm/string.hpp>
#include <string>
#include <vector>

using namespace utility::hex;
using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct CatalogIndexRecord
{
  HFSPlusCatalogKey key;
  uint32_t pointer;
};

struct CatalogLeafRecord
{
  HFSPlusCatalogKey  key;
  HFSPlusCatalogData data;
};

struct CatalogTreeNode
{
  int type;
  vector<CatalogIndexRecord> irecs;
  vector<CatalogLeafRecord>  lrecs;
};

class CatalogTree;

template <> 
struct BTreeTraits<CatalogTree>
{
  typedef CatalogTreeNode Node;
  typedef HFSPlusCatalogKey SearchKey;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class CatalogTree: public BTree<CatalogTree>
{
public:
  CatalogTree(HFSFile* file);
  ~CatalogTree();
  
public:
  int compare_keys(HFSPlusCatalogKey const& key1, ByteBuffer& key2) const
  {
    return -1;
  }

  auto read_index_record(ByteBuffer& buffer, uint32_t offset) -> CatalogIndexRecord
  {
    CatalogIndexRecord record;
    record.key.read_from(buffer);
    record.pointer = buffer.get_uint4_be();

    return record;
  }

  auto read_leaf_record(ByteBuffer& buffer, uint32_t offset) -> CatalogLeafRecord
  {
    CatalogLeafRecord record;
    record.key.read_from(buffer);
    record.data.read_from(buffer);

    return record;
  }
  
  void print_leaf(BufferPair const&)
  {
  }

protected:
  // REMARK찾고자 하는 파일의 CNID를 아는 경우
  // 1. search CatalogKey.parientID = cnid, name = "" -> thread record
  // 2. search CatalogKey.parentId = thread.parentID, name=thread.name
  auto search_by_cnid(HFSCatalogNodeID cnid) -> HFSPlusCatalogFile
  {
    HFSPlusCatalogFile file;
    HFSPlusCatalogKey key;
    
    // search(key);
    
    return file;
  }
  
  void get_record_from_path(string const& path)
  {
    if (!boost::starts_with(path, "/"))
      return;
      // return make_pair(nullptr, nullptr);
    
    if (path == "/")
      return;
    
    search_by_cnid(kHFSRootFolderID);
    
    // TODO
    
  }
  
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
