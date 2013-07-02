{-
fibonacci.hs

Created by Yamato Matsuoka on 2013-07-02.

# Description
The Fibonacci series is defined as: F(0) = 0; F(1) = 1; F(n) = F(n-1) + F(n-2) when n>1;. Given a positive integer 'n', print out the F(n).

## Input sample
The first argument will be a text file containing a positive integer, one per line. e.g. 
```
5
12
```

## Output sample
Print to stdout, the fibonacci number, F(n). e.g.
```
5
144
```
-}

import System.Environment (getArgs)

fibonacci :: Int -> Int
fibonacci n = fibs !! n

fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = map read $ lines contents
    let outputs = map fibonacci inputs
    mapM print outputs

