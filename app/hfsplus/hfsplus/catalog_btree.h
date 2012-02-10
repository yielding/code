#ifndef CATALOG_BTREE_H_53HYMVEK
#define CATALOG_BTREE_H_53HYMVEK

#include "btree.h"

#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef BTreeRecord<HFSPlusCatalogKey, HFSPlusCatalogData> CatalogRecord;
typedef BTreeNode<CatalogRecord> CatalogNode;

class CatalogTree;

template <> 
struct BTreeTraits<CatalogTree>
{
  typedef CatalogNode Node;
  typedef CatalogRecord Record;
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
  auto compare_keys(HFSPlusCatalogKey const& key1, HFSPlusCatalogKey const& key2) const 
    -> int;

  auto metadata_dir_id() -> HFSCatalogNodeID;
  auto search_by_cnid(HFSCatalogNodeID cnid) -> CatalogRecord;
  
public:
  auto get_record_from_path(string const& path) -> CatalogRecord;
  
  auto get_folder_contents(HFSCatalogNodeID folderID) -> CatalogNode;

};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
