{-
Pascals Triangle
=================
Description
------------
A pascals triangle row is contructed by looking at the previous row and adding the numbers to its left and right to arrive at the new value. If either the number to its left/right is not present, substitute a zero in it's place. More details can be found here: http://en.wikipedia.org/wiki/Pascal's_triangle. e.g. A Pascals triangle upto a depth of 6 can be shown as:

                1
              1   1
            1   2   1
           1  3   3   1
         1  4   6   4   1
        1  5  10  10  5   1

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a positive integer which indicates the depth of the triangle (1 based). e.g.
```
6
```

Output sample
--------------
Print out the resulting pascal triangle upto the requested depth in row major form e.g.
```
1 1 1 1 2 1 1 3 3 1 1 4 6 4 1 1 5 10 10 5 1
```
-}

import System.Environment (getArgs)

pascalNext :: [Int] -> [Int]
pascalNext xs = zipWith (+) (0:xs) (xs++[0])

pascalSequence :: Int -> [Int]
pascalSequence n
    | n < 0     = []
    | otherwise = concat $ take n $ iterate pascalNext [1]

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map read $ lines contents
    let outputs = map pascalSequence inputs
    mapM putStrLn [unwords (map show out) | out <- outputs]
