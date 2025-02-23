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
    println("[{}, {}]", p.first, p.second);

  return 0;
}
