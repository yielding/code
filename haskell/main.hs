module Main (main) where

import Tree (Tree(Leaf,Branch), fringe) 

main = do 
  print $ fringe (Branch (Leaf 1) (Leaf 2))