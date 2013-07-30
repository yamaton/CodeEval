{-
Sudoku 
=======
Description
------------
Sudoku is a number-based logic puzzle. It typically comprises of a 9*9 grid with digits so that each column, each row and each of the nine 3*3 sub-grids that compose the grid contains all the digits from 1 to 9. For this challenge, you will be given an N*N grid populated with numbers from 1 through N and you have to determine if it is a valid sudoku solution. You may assume that N will be either 4 or 9. The grid can be divided into square regions of equal size, where the size of a region is equal to the square root of a side of the entire grid. Thus for a 9*9 grid there would be 9 regions of size 3*3 each.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains the value of N, a semicolon and the sqaure matrix of integers in row major form, comma delimited. e.g.
```
4;1,4,2,3,2,3,1,4,4,2,3,1,3,1,4,2
4;2,1,3,2,3,2,1,4,1,4,2,3,2,3,4,1
```

Output sample
--------------
Print out True/False if the grid is a valid sudoku layout. e.g.
```
True
False
```
-}

import System.Environment (getArgs)
import Data.List (sort, transpose)

type Sudoku = [[Int]]
isValidSudoku :: Sudoku -> Bool
isValidSudoku sudoku = rowCheck && colCheck && blockCheck
  where
    n = length sudoku
    theSet = [1..n]
    rowCheck = all (\row -> sort row == theSet) sudoku
    colCheck = all (\col -> sort col == theSet) (transpose sudoku)
    chunkRow = reshapeBy n sudoku
    blocks = concatMap (concat . reshapeBy n . transpose) chunkRow
    blockCheck = all (\bl -> sort bl == theSet) blocks


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


reshapeBy :: Int -> [a] -> [[a]]
reshapeBy n xs = 
  case splitAt n xs of
    ([], _)  -> []
    (ys,zs)  -> ys : reshapeBy n zs


reader :: String -> Sudoku
reader s = reshapeBy n numbers
  where 
    [nStr, sss] = split ';' s
    n = read nStr
    numbers = map read $ split ',' sss


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map reader $ lines contents
    let outputs = map isValidSudoku inputs
    mapM_ print outputs

