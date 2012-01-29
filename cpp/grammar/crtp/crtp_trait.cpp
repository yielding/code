#include <iostream>

using namespace std;

// file a

template <typename T> 
struct BTreeTraits;

template <typename HFSTree> 
struct BTree
{
  typedef typename BTreeTraits<HFSTree>::Data Data;
};

// file b
class CatalogTree;

template <> 
struct BTreeTraits<CatalogTree>
{
  struct Data {
    int a;
  };
};

class CatalogTree: public BTree<CatalogTree>
{
};

// file c
int main(int argc, char const* argv[])
{
  CatalogTree c;
  CatalogTree::Data d;
  d.a = 10;
  
  return 0;
}
