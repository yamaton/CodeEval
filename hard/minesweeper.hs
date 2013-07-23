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

mineSweep :: Int -> Int -> String -> String


def minesweep(row, col, mines):
    x = add_wall(row, col, mines)
    return ["".join(count_mines(i, j, x) for j in range(1, col+1))  
                for i in range(1, row+1)]


def add_wall(row, col, mines):
    mines_with_wall = ['.' + line + '.' for line in mines]
    wall = ['.' * (col + 2)]
    mines_with_wall = wall + mines_with_wall + wall
    return mines_with_wall


def count_mines(i, j, mines):
    m = mines
    if m[i][j] == '*':
        return "*"
    else:
        surroundings = "".join([line[j-1:j+2] for line in m[i-1:i+2]])
        return str(surroundings.count('*'))


countMines :: (Int, Int) -> [String] -> Char
countMines (i, j) xxs
  | m !! i !! j == '*' = '*'
  | otherwise          = 


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


reader :: String -> (Int, Int, String)
reader s = (m, n, latter) where
  (former, latter) = split ';' s
  [m, n] = map read (split ',' former)


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map reader $ lines contents
  let outputs = [mineSweep m n s | (m, n, s) <- inputs]
  mapM_ putStrLn outputs
