{-
sum_digits.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
Given a positive integer, find the sum of its constituent digits.

Input sample
-------------
The first argument will be a text file containing positive integers, one per line. e.g. 
```
23
496
```

Output sample
--------------
Print to stdout, the sum of the numbers that make up the integer, one per line. e.g.
```
5
19
```
-}

import System.Environment (getArgs)

integerDigits :: Int -> [Int]
integerDigits n = map (read . (:[])) (show n)

sumDigits :: Int -> Int
sumDigits = sum . integerDigits

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map read $ lines contents
  let outputs = map sumDigits inputs
  mapM print outputs
