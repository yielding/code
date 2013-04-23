#include "AVLTree.h"

#include <iostream>
#include <boost/format.hpp>

using namespace std;
using namespace boost;

int main(int argc, const char *argv[])
{
  AVLTree<string, int> tree;

  tree.insert("leech", 42);
  tree.insert("kamin", 40);
  tree["gunhee"] = 10;

  cout << "size: "  << tree.size() << endl;
  for (auto p : tree)
      cout << str(format("[%s, %d]") % p.first % p.second) << endl;

  return 0;
}
