{-
Largest Common Substring
========================
Created by Yamato Matsuoka on 2013-07-12.

Description
------------
You are given two sequences. Write a program to determine the longest common subsequence between the two strings(each string can have a maximum length of 50 characters). (NOTE: This subsequence need not be contiguous. The input file may contain empty lines, these need to be ignored.)

Input sample
-------------
The first argument will be a file that contains two strings per line, semicolon delimited. You can assume that there is only one unique subsequence per test case. e.g.
```
XMJYAUZ;MZJAWXU
```

Output sample
-------------
The longest common subsequence. Ensure that there are no trailing empty spaces on each line you print. e.g.
```
MJAU
```

[Comment]
    ver 0.9 ... lcs using Data.List (subsequences) is too slow to pass CodeEval.
-}

import System.Environment (getArgs)
import Data.Array (Array, array, (!))

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where (w, s'') = break (== c) s'


lcsLengthMatrix :: String -> String -> Array (Int, Int) Int
lcsLengthMatrix s t = mat
  where 
    (lenS, lenT) = (length s, length t)
    mat = array ((0,0),(lenS,lenT)) 
       (   [((i,0),0) | i <- [0 .. lenS]] 
        ++ [((0,j),0) | j <- [0 .. lenT]] 
        ++ [(
          (i+1,j+1),
          if s !! i == t !! j 
            then (mat ! (i,j)) + 1
            else max (mat ! (i+1, j)) (mat ! (i, j+1))
            )
          | i <- [0..lenS-1], j <- [0..lenT-1]] )


backtrack :: Array (Int, Int) Int -> String -> String -> Int -> Int -> String
backtrack ar s1 s2 i j
  | i == -1 || j == -1          = ""
  | s1 !! i == s2 !! j          = backtrack ar s1 s2 (i-1) (j-1) ++ [s1 !! i]
  | ar ! (i+1,j) > ar ! (i,j+1) = backtrack ar s1 s2  i    (j-1)
  | otherwise                   = backtrack ar s1 s2 (i-1)  j


lcs :: String -> String -> String
lcs s t = backtrack mat s t (length s - 1) (length t - 1)
  where mat = lcsLengthMatrix s t


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map (splitOn ';') $ lines contents
  let outputs = [lcs s t | [s,t] <- inputs]
  mapM_ putStrLn outputs
