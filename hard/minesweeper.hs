{-
Minesweeper
===========
Description
-----------
You will be given an M*N matrix. Each item in this matrix is either a '*' or a '.'. A '*' indicates a mine whereas a '.' does not. The objective of the challenge is to output a M*N matrix where each element contains a number (except the positions which actually contain a mine which will remain as '*') which indicates the number of mines adjacent to it. Notice that each position has at most 8 adjacent positions e.g. left, top left, top, top right, right, ...

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line in this file contains M,N, a semicolon and the M*N matrix in row major form. e.g.
```
3,5;**.........*...
4,4;*........*......
```

Output sample
--------------
Print out the new M*N matrix (in row major form) with each position(except the ones with the mines) indicating how many adjacent mines are there. e.g.
```
**100332001*100
*10022101*101110
```

**...
.....
.*...


-}

import System.Environment (getArgs)

type Board = [[Char]]
type Coord = (Int, Int)

mineSweep :: Board -> Int -> Int -> String
mineSweep board m n = [cellInfo board (i, j) | i <- [0 .. m-1], j <- [0 .. n-1]]

cellInfo :: Board -> Coord -> Char
cellInfo board (i, j) 
  | get board (i,j) == '*' = '*'
  | otherwise              = charN
    where charN:_ = show $ countMine board (i, j)

get ::  Board -> Coord -> Char
get board (i,j) = board !! i !! j

countMine :: Board -> Coord -> Int
countMine board (i, j) = length $ filter (== '*') $ map (get board) neighbours
    where (m, n) = (length board, length (head board))
          neighbours = [(p,q) | p <- [i-1,i,i+1], q <- [j-1,j,j+1],
                                0 <= p, p < m, 0 <= q, q < n]

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s'' where 
    (w, s'') = break (== c) s'


reshapeBy :: Int -> [a] -> [[a]]
reshapeBy n xs = 
  case splitAt n xs of
    ([], _)  -> []
    (ys,zs)  -> ys : reshapeBy n zs


reader :: String -> (Int, Int, Board)
reader s = (m, n, board) where
  [former, latter] = split ';' s
  [m, n] = map read (split ',' former)
  board = reshapeBy n latter


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map reader $ lines contents
  let outputs = [mineSweep b m n | (m, n, b) <- inputs]
  mapM_ putStrLn outputs
