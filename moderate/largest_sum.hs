{-
largest_sum.hs

Created by Yamato Matsuoka on 2013-07-05.

Description
------------
Write a program to determine the largest sum of contiguous integers in a list.

Input sample
-------------
The first argument will be a text file containing a comma separated list of integers, one per line. e.g. 
```
-10, 2, 3, -2, 0, 5, -15
2,3,-2,-1,10
```

Output sample
--------------
Print to stdout, the largest sum. In other words, of all the possible contiguous subarrays for a given array, find the one with the largest sum, and print that sum. e.g.
```
8
12
```

-}

import System.Environment (getArgs)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s'' where (w, s'') = break (== c) s'

contiguousMaxSum :: [Int] -> Int
contiguousMaxSum xs = maximum $ map sum [drop i (take j xs) | j <- [1 .. len], i <- [0 .. (j-1)]] 
    where len = length xs

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = [map read (split ',' line) | line <- lines contents]
    let outputs = map contiguousMaxSum inputs
    mapM print outputs



