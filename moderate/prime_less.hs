{-
double_squares.hs

Created by Yamato Matsuoka on 2012-07-08.

Description
------------
Print out the prime numbers less than a given number N. For bonus points your solution should run in N*(log(N)) time or better. You may assume that N is always a positive integer.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 4,294,967,295. eg.
```
10
20
100
```

Output sample
--------------
For each line of input, print out the prime numbers less than N, in ascending order, comma delimited. (There should not be any spaces between the comma and numbers) eg.
```
2,3,5,7
2,3,5,7,11,13,17,19
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
```
-}

import System.Environment (getArgs)
import Data.List (intercalate)

sieve :: Int -> [Int]
sieve n = 

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map read $ lines contents
    let outputs = map sieve inputs
    mapM putStrLn $ [(intercalate ",") (map read out) | out <- outputs]
