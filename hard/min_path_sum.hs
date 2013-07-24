{-
Minimum Path Sum 
=================
Description
-----------
You are given an n*n matrix of integers. You can move only right and down. Calculate the minimal path sum from the top left to the bottom right

Input sample
-------------
Your program should accept as its first argument a path to a filename. The first line will have the value of n(the size of the square matrix). This will be followed by n rows of the matrix. (Integers in these rows will be comma delimited). After the n rows, the pattern repeats. e.g.
```
2
4,6
2,8
3
1,2,3
4,5,6
7,8,9
```

Output sample
---------------
Print out the minimum path sum for each matrix. e.g.
```
14
21
```
-}

import System.Environment (getArgs)

minPathSum :: [[Int]] -> Int
minPathSum matrix = helper matrix (n-1) (n-1)
    where n = length matrix

helper :: [[Int]] -> Int -> Int -> Int
helper matrix i j
  | isOutside i j = 100000
  | isTopLeft i j = matrix !! 0 !! 0
  | otherwise     = (matrix !! i !! j) + min (helper matrix (i-1) j) (helper matrix i (j-1))
    where
      n = length matrix
      isOutside p q = p < 0 || q < 0 || p >= n || q >= n 
      isTopLeft p q = p == 0 && q == 0


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


reader :: [String] -> [[[Int]]]
reader [] = []
reader (x:xs) = [map read ss | ss <- map (split ',') former] : reader latter
  where
    (former, latter) = splitAt (read x) xs


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = reader (lines contents)
  let outputs = map minPathSum inputs
  mapM_ print outputs
