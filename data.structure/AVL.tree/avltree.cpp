#include "avltree.h"

#include <print>

using namespace std;

int main()
{
  AVLTree<string, int> tree;

  tree.insert("leech", 42);
  tree.insert("kamin", 40);
  tree["gunhee"] = 10;

  println("size: {}", tree.size());
  for (auto p : tree)
    print("[{}, {}] ", p.first, p.second);

  println("");

  for (auto it=tree.begin(); it != tree.end(); ++it)
    print("[{}, {}] ", it->first, it->second);

  return 0;
}
