{-
num_of_ones.hs

Created by Yamato Matsuoka on 2013-07-06.

Description
------------
Write a program to determine the number of 1 bits in the internal representation of a given integer.

Input sample
-------------
The first argument will be a text file containing an integer, one per line. e.g. 
```
10
22
56
```

Output sample
--------------
Print to stdout, the number of ones in the binary form of each number. e.g.
```
2
3
3
```
-}

import System.Environment (getArgs)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

decToBin :: Int -> String
decToBin n = showIntAtBase 2 intToDigit n ""

numOfOnes :: Int -> Int
numOfOnes n = length $ filter (== '1') (decToBin n)

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = map read $ lines contents
    let ouputs = map numOfOnes inputs
    mapM print ouputs

