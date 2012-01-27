#ifndef CATALOG_BTREE_H_53HYMVEK
#define CATALOG_BTREE_H_53HYMVEK

#include "btree.h"

#include <boost/algorithm/string.hpp>
#include <string>

using namespace utility::hex;
using namespace std;
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
  ByteBuffer parse_key(ByteBuffer& b, uint16_t& offset) const
  {
    return b;
  }
  
  ByteBuffer parse_data(ByteBuffer& b, uint16_t& offset) const
  {
    return b;
  }
  
  void print_leaf(BufferPair const&)
  {
  }

protected:
  search_by_cnid(HFSCatalogNodeID cnid)
  {
    auto thr_record = search
  }
  
  void get_record_from_path(string const& path)
  {
    if (!boost::starts_with(path, "/"))
      return;
      // return make_pair(nullptr, nullptr);
    
    if (path == "/")
      return search_by_cnid(kHFSRootFolderID);
    
    // TODO
    
  }
  
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
