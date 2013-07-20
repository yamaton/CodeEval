{-
Lowest Common Ancestor

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

chain :: Int -> [Int]
chain n = takeWhile (>0) $ iterate parent n 
  where
    parent 10 = 20
    parent 29 = 20
    parent 20 =  8
    parent  3 =  8
    parent  8 = 30
    parent 52 = 30
    parent _  =  0


lowestCommonAncestor :: Int -> Int -> Int
lowestCommonAncestor p q = head [x | x <- pChain, x `elem` qChain]
  where [pChain, qChain] = map chain [p, q]


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = [map read (words line) | line <- lines contents, (not . null) line]
    let outputs = [lowestCommonAncestor p q | [p, q] <- inputs]
    mapM_ print outputs

