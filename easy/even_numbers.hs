{-
Challenge Description
======================
Write a program which checks input numbers and determines whether a number is even or not.

## Input sample

Your program should accept as its first argument a path to a filename. Input example is the following
```
701
4123
2936
```
## Output sample:

Print 1 if the number is even or 0 if the number is odd.
```
0
0
1
```
All numbers in input are integers > 0 and < 5000.
-}

import System.Environment (getArgs)

isEven :: Int -> Int
isEven n
    | even n    = 1
    | otherwise = 0

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = map read $ lines contents
    let outputs = map isEven inputs
    mapM print outputs
