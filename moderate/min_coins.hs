{-
Minimum Coins
==============

    Created by Yamato Matsuoka on 2013-07-06

Description
-----------
You are given 3 coins of value 1, 3 and 5. You are also given a total which you have to arrive at. Find the minimum number of coins to arrive at it.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a positive integer number which represents the total you have to arrive at e.g.
```
11
20
```

Output sample
--------------
Print out the minimum number of coins required to arrive at the total e.g.
```
3
4
```
-}

import System.Environment (getArgs)

minCoins :: Int -> Int
minCoins n = (n `div` 5) + (nMod5 `div` 3) + (nMod5 `mod` 3)
    where nMod5 = n `mod` 5

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = map read $ lines contents
    let outputs = map minCoins inputs
    mapM print outputs

