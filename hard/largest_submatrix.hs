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
type :: Matrix = Int -> Int -> Int

integralTransform :: Matrix -> Matrix
integralTransform mat = [[(sum . concat . (take j) . transpose . (take i))  mat | j <- [1..n]] | i <- [1..n]]
  where n = length mat

largestSubmatrix :: Matrix -> Int
largestSubMatrix mat = last . sort $ [subMatrixSum xL xH yL yH | [xL, xH, yL, yH] <- genIndices]
  where
    n = length mat
    integMat = integralTransform mat
    subMatrixSum xL xH yL yH = integMat xH yH - integMat xH yL - integMat xL yH + integMat xL yL



main = do 
  f:_ <- getArgs
  contents <- readFile f
  let input = [map read (words line) | line <- lines contents]
  let output = largestSubMatrix input
  print output
