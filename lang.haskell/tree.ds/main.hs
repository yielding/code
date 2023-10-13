module Main (main) 
where

import Tree (Tree(..), 
             Functor2(..), 
             fringe, 
             treeMap, 
             treeFold)

doubleTree = treeMap (*2)
sumTree = treeFold (+) id
fringeTree = treeFold (++) (: [])

tree1 = (Branch (Leaf 1) (Leaf 2))
tree2 =
  Branch
    (Branch
      (Branch
        (Leaf 1)
        (Branch (Leaf 2) (Leaf 3)))
      (Branch 
        (Leaf 4)
        (Branch (Leaf 5) (Leaf 6))))
    (Branch
      (Branch (Leaf 7) (Leaf 8))
      (Leaf 9))

main = do 
  -- print $ fringe tree1 
  -- print $ doubleTree tree1
  -- print $ fringeTree tree1

  print $ doubleTree tree2
  print $ fringeTree tree2

  print $ fmap2 (*2) tree1