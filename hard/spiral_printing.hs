{-
Spiral Printing
===============
Challenge Description
---------------------
Write a program to print a 2D array (n x m) in spiral order (clockwise)

Input sample
-------------
Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains three items (semicolon delimited). The first is 'n'(rows), the second is 'm'(columns) and the third is a single space separated list of characters/numbers in row major order. eg.
```
3;3;1 2 3 4 5 6 7 8 9
```

Output sample
--------------
Print out the matrix in clockwise fashion, one per line, space delimited. eg.
```
1 2 3 6 9 8 7 4 5
```

-}

import System.Environment (getArgs)


reshapeBy :: Int -> [a] -> [[a]]
reshapeBy n xs = 
  case splitAt n xs of
    ([], _)  -> []
    (ys,zs)  -> ys : reshapeBy n zs

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


spiral :: Int -> Int -> [a] -> [a]
spiral rowN colN numbers = helper [(0,0)] 0
  where
    matrix = reshapeBy colN numbers 
    delta = cycle [(0,1), (1,0), (0,-1), (-1,0)]
    --helper :: [(Int,Int)] -> Int -> [a]
    helper path@((x,y):past) deltaIx
      | (x, y) `elem` past = [matrix !! i !! j | (i, j) <- reverse past]
      | isKeepGoing       = helper ((newX,newY):path) deltaIx
      | otherwise         = helper (modNextPos:path) (deltaIx + 1)
        where
          (dx, dy)= delta !! deltaIx
          (newX,newY) = (x + dx, y + dy)
          isKeepGoing = (0 <= newX) && (newX < rowN) && (0 <= newY) && (newY < colN) &&
                        (newX, newY) `notElem` past
          (dx', dy') = delta !! (deltaIx + 1)                        
          modNextPos = (x + dx', y + dy')                        


reader :: String -> ((Int, Int), [String])
reader s = ((read s1, read s2), words xs)
  where [s1, s2, xs] = split ';' s


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map reader $ lines contents
  let outputs = [spiral rowN colN numbers | ((rowN, colN), numbers) <-  inputs]
  mapM_ (putStrLn . unwords) outputs

