{-
Distinct Subsequences
=======================
Challenge Description
----------------------
A subsequence of a given sequence S consists of S with zero or more elements deleted. Formally, a sequence Z = z1z2..zk is a subsequence of X = x1x2...xm, if there exists a strictly increasing sequence <i1,i2...ik> of indicies of X such that for all j=1,2,...k we have Xij = Zj. e.g. Z=bcdb is a subsequence of X=abcbdab with corresponding index sequence <2,3,5,7>

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated strings. The first is the sequence X and the second is the subsequence Z. e.g.
```
babgbag,bag
rabbbit,rabbit
```

Output sample
--------------
Print out the number of distinct occurrences of Z in X as a subsequence e.g.
```
5
3
```
-}

import System.Environment (getArgs)

-- >>> combinations 2 [1 .. 4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
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


countSubsequences :: String -> String -> Int
countSubsequences s t = length $ filter (== t) (combinations (length t) s)

reader :: String -> (String, String)
reader s = (former, latter)
  where (former, _:latter) = break (== ',') s

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map reader $ lines contents
  let outputs = [countSubsequences s t | (s, t) <- inputs]
  mapM_ print outputs

