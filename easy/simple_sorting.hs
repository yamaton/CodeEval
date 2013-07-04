{-
simple_sorting.hs

Created by Yamato Matsuoka on 2013-07-03.

Challenge Description
=====================
Write a program which sorts numbers.

## Input sample
Your program should accept as its first argument a path to a filename. Input example is the following
```
70.920 -38.797 14.354 99.323 90.374 7.581
-37.507 -3.263 40.079 27.999 65.213 -55.552
```

## Output sample:
Print sorted numbers in the following way.
```
-38.797 7.581 14.354 70.920 90.374 99.323
-55.552 -37.507 -3.263 27.999 40.079 65.213
```
-}

import System.Environment (getArgs)
import Data.List (sortBy)

myCompare :: String -> String -> Ordering
myCompare s1 s2 = compare (read s1 :: Float) (read s2 :: Float)

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [words line | line <- lines contents]
    let outputs = [sortBy myCompare nums | nums <- inputs]
    mapM putStrLn (map unwords outputs)


