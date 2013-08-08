{-
Shortest Repetition 
====================

Challenge Description
---------------------
Write a program to determine the shortest repetition in a string. 
A string is said to have period p if it can be formed by concatenating one or more repetitions of another string of length p. For example, the string "xyzxyzxyzxyz" has period 3, since it is formed by 4 repetitions of the string "xyz". It also has periods 6 (two repetitions of "xyzxyz") and 12 (one repetition of "xyzxyzxyzxyz").

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line will contain a string of up to 80 non-blank characters. E.g.
```
abcabcabcabc
bcbcbcbcbcbcbcbcbcbcbcbcbcbc
dddddddddddddddddddd
```

Output sample
-------------
Print out the smallest period of the input string. E.g.
```
3
2
1
```

-}

import System.Environment (getArgs)
import Data.List (elemIndices)

-- modified from "cycle_detection.hs"
periodicityLength :: Eq a => [a] -> Int
periodicityLength []        = 1
periodicityLength [x]       = 1
periodicityLength ys@(x:xs) = if (null out || 2 * (head out) > len) then len else (head out)
  where
    len = length ys
    _:idx = elemIndices x ys
    isPeriodicEvery p = all (\i -> ys !! i == ys !! (i+p)) [0 .. len - p - 1]
    out = dropWhile (not . isPeriodicEvery) idx

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = lines contents
  let outputs = map periodicityLength inputs
  mapM_ print outputs


