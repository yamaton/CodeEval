{-
pass_triangle.hs

Challenge Description
======================
By starting at the top of the triangle and moving to adjacent 
numbers on the row below, the maximum total from top to bottom is 27.
```
   5
  9 6
 4 6 8
0 7 1 5
```
5 + 9 + 6 + 7 = 27

## Input sample
Your program should accept as its first argument a path to a filename. 
Input example is the following
```
5
9 6
4 6 8
0 7 1 5
```
You make also check full input file which will be used for your code evaluation.

## Output sample
The correct output is the maximum sum for the triangle. 
So for the given example the correct answer would be
```
27
```
-}

import System.Environment (getArgs)

addNeighbourMax :: [Int] -> [Int] -> [Int]
addNeighbourMax xs acc = zipWith (+) xs (zipWith max (tail acc) (init acc))

maxPassTriangle :: [[Int]] -> Int
maxPassTriangle xxs = head $ foldr1 addNeighbourMax xxs

main = do
  f:_ <- getArgs
  contents <- readFile f
  let input = [map read (words line) | line <- lines contents]
  print $ maxPassTriangle input
  