#include "SplayTree.h"

#include <iostream>
#include <print>

using namespace std;

int main(int argc, const char *argv[])
{
  SplayTree<string, int> tree;

  tree.insert("leech", 42);
  tree.insert("kamin", 40);
  tree["gunhee"] = 10;

  println("size: {}", tree.size());
  for (auto& [name, age] : tree)
    println("[{}, {}]", name, age);

  return 0;
}
