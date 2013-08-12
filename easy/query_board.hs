{-
query_board.hs
Created: 2013-07-03 by Yamato Matsuoka

Query Board
============

Description
-----------
There is a board (matrix). Every cell of the board contains one integer, which is 0 initially.

The next operations can be applied to the Query Board:
SetRow i x: it means that all values in the cells on row "i" have been changed to value "x" after this operation.
SetCol j x: it means that all values in the cells on column "j" have been changed to value "x" after this operation.
QueryRow i: it means that you should output the sum of values on row "i".
QueryCol j: it means that you should output the sum of values on column "j".

The board's dimensions are 256 x 256
"i" and "j" are integers from 0 to 255
"x" is an integer from 0 to 31

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains an operation of a query. E.g.
```
SetCol 32 20
SetRow 15 7
SetRow 16 31
QueryCol 32
SetCol 2 14
QueryRow 10
SetCol 14 0
QueryRow 15
SetRow 10 1
QueryCol 2
```

Output sample
--------------
For each query, output the answer of the query. E.g.
```
5118
34
1792
3571
```
-}

---- [TODO] make the data structure into Array to speed up the computation

import System.Environment (getArgs)
import Data.List (transpose)
import Control.Monad (forM_)
import Control.Monad.State (State, runState, evalState, get, put)

data Command = SetCol | SetRow | QueryCol | QueryRow deriving (Read, Show)

initialBoard :: [[Int]]
initialBoard = replicate 256 $ replicate 256 0

queryBoard :: [(Command, [Int])] -> [Int]
queryBoard commands = flip evalState (initialBoard, []) $ do
  forM_ commands $ \(com, xs) -> do
    (board, out) <- get
    case (com, xs) of 
      (SetRow, [i,x]) -> put (setRow i x board, out)
      (SetCol, [i,x]) -> put (setCol i x board, out)
      (QueryRow, [i]) -> put (board, queryRow i board : out)
      (QueryCol, [i]) -> put (board, queryCol i board : out)
  (board, out) <- get
  return out

setRow :: Int -> Int -> [[Int]] -> [[Int]]
setRow i x mat = take i mat ++ [replicate m x] ++ drop (i + 1) mat
  where m = length (head mat)

setCol :: Int -> Int -> [[Int]] -> [[Int]]
--setCol i x mat = transpose $ setRow i x (transpose mat)   ---- Data.List.transpose is slow!
setCol i x mat = [take i xs ++ (x : drop (i + 1) xs) | xs <- mat]

queryRow :: Int -> [[Int]] -> Int
queryRow i mat = sum $ mat !! i

queryCol :: Int -> [[Int]] -> Int
queryCol j mat = queryRow j (transpose mat)

parser :: String -> (Command, [Int])
parser s = (read w, map read ws)
    where w:ws = words s

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map parser $ lines contents
  let outputs = queryBoard inputs
  mapM_ print $ reverse outputs
