{-
Challenge Description
======================

Write a program which swaps letters' case in a sentence. All non-letter characters should remain the same.

## Input sample
Your program should accept as its first argument a path to a filename. Input example is the following
```
Hello world!
JavaScript language 1.8
A letter
```

## Output sample
Print results in the following way.
```
hELLO WORLD!
jAVAsCRIPT LANGUAGE 1.8
a LETTER
```
-}

import System.Environment (getArgs)
import Data.Char (isLower, isUpper, toUpper, toLower)

swapCase :: Char -> Char
swapCase c
    | isLower c = toUpper c
    | isUpper c = toLower c
    | otherwise = c

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let output = map swapCase contents
    putStrLn output
