{-
Challenge Description
======================
Write a program which finds the next-to-last word in a string.

## Input sample
Your program should accept as its first argument a path to a filename. Input example is the following
```
some line with text
another line
```
Each line has more than one word.

## Output sample
Print the next-to-last word in the following way.
```
with
another
```
-}

import System.Environment (getArgs)

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [words line | line <- lines contents]
    let outputs = map (last . init) inputs
    mapM putStrLn outputs
