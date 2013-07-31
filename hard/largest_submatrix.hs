{-
Lagest Sub-Matrix
==================
Challenge Description
----------------------
You have a matrix of positive and negative integers. Find a sub-matrix with the largest sum of it's elements. There is no limitation for sub-matrix dimensions. It only has to be rectangular. 
* Each element in the matrix is in range [-100, 100]. 
* Input matrix has an equal number of rows and columns.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Read the matrix from this file. Example of the matrix is the following:
```
-1 -4 -5 -4
-5 8 -1 3
-2 1 3 2
1 5 6 -9
```

* rows are separated by new-line character, columns are separated by space char. 
* it's up to 20 rows/columns in the input file. 

After some calculations you may find that the sub-matrix with the largest sum for the input is
```
8 -1
1 3
5 6
```

Output sample
--------------
Print out the sum of elements for the largest sub-matrix. For the given input it is
```
22
```
-}

import System.Environment (getArgs)
import Control.Monad (replicateM)
import Data.List (transpose, sort)

type Matrix = [[Int]]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs  
  | n == 1    = map (:[]) xs 
  | otherwise = helper n (length xs) xs    
    where
      helper k l ys@(z:zs)        
        | k < l     = map (z :) (combinations (k-1) zs)
                         ++ combinations k zs
        | k == l    = [ys]
        | otherwise = []

-- |
-- >>> integralTransform [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,3,6],[5,12,21],[12,27,45]]
integralTransform :: Matrix -> Matrix
integralTransform = transpose . map (scanl1 (+)) . transpose . map (scanl1 (+))

-- |
-- 
largestSubMatrix :: Matrix -> Int
largestSubMatrix mat = maximum [subMatrixSum xL xH yL yH | [xL, xH, yL, yH] <- genIndices n]
  where
    n = length mat
    table = integralTransform mat
    subMatrixSum xL xH yL yH = table !! xH !! yH + table !! xL !! yL - table !! xH !! yL - table !! xL !! yH

-- |
-- >>> genIndices 3
-- [[0,1,0,1],[0,1,0,2],[0,1,1,2],[0,2,0,1],[0,2,0,2],[0,2,1,2],[1,2,0,1],[1,2,0,2],[1,2,1,2]]
genIndices :: Int -> [[Int]]
genIndices n = map concat $ replicateM 2 $ combinations 2 [0..(n-1)]


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let input = [map read (words line) | line <- lines contents]
  print $ largestSubMatrix input
