{-
lca.hs

Created by Yamato Matsuoka on 2013-07-05.

Description
-----------
Write a program to determine the lowest common ancestor of two nodes in a binary search tree. You may hardcode the following binary search tree in your program:

    30
    |
  ____
  |   |
  8   52
  |
____
|   |
3  20
    |
   ____
  |   |
  10 29
  

Input sample
-------------
The first argument will be a text file containing 2 values that represent two nodes within the tree, one per line. e.g. 
```
8 52
3 29
```

Output sample
--------------
Print to stdout, the least common ancestor, one per line. e.g.
```
30
8
```
-}

import System.Environment (getArgs)

lowestCommonAncestor :: Int -> Int -> Int
lowestCommonAncestor p q = 

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = [map read (words line) | line <- lines contents]
    let outputs = [lowestCommonAncestor p q | [p, q] <- inputs]
    mapM print outputs

