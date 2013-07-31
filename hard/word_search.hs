{-
Word Search
===========

Description
-----------
Given a 2D board and a word, find if the word exists in the grid. The word can be constructed from letters of sequentially adjacent cell, where adjacent cells are those horizontally or vertically neighboring. The same letter cell may not be used more than once.

Input sample
-------------
The board to be used may be hard coded as:

[
[ABCE],
[SFCS],
[ADEE]
]

Your program should accept as its first argument a path to a filename. Each line in this file contains a word. e.g.
```
ASADB
ABCCED
```

Output sample
--------------
Print out True if the word exists in the board, False otherwise. e.g.
```
False
True
```

-}
import System.Environment (getArgs)
import Data.List (elemIndices)

type Position = (Int, Int)

searchWord :: [String] -> String -> Bool
searchWord field (w:word) = or [isReachable field start word | start <- findStartPos field w]

isReachable :: Eq a => [[a]] -> Position -> [a] -> Bool
isReachable field pos x = helper [pos] x
  where
    helper _ [] = True
    helper ((i,j):past) (x:xs) = 
      or [ helper (p:(i,j):past) xs |
             p <- allowed field [(i-1,j),(i+1,j),(i,j-1),(i,j+1)], 
             p `notElem` past,
             (field `at` p) == x ] 
    helper [] _ = error "something wrong!"

at :: [[a]] -> Position -> a
at field (i,j) = field !! i !! j

allowed :: [[a]] -> [Position] -> [Position]
allowed field = filter conditions
  where rows = length field
        cols = length $ head field
        conditions = \(i, j) -> (0 <= i && i < rows && 0 <= j && j < cols)

findStartPos :: Eq a => [[a]] -> a -> [Position]
findStartPos field x = map (\n -> (n `div` cols, n `mod` cols)) flatIndices
  where cols = length (head field)
        flatIndices = elemIndices x (concat field)

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = lines contents
  let field = ["ABCE","SFCS","ADEE"]
  let outputs = map (searchWord field) inputs
  mapM_ print outputs