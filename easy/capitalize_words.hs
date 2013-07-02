{-
Challenge Description
======================
Write a program which capitalizes words in a sentence.

## Input sample
Your program should accept as its first argument a path to a filename. Input example is the following
```
Hello world
javaScript language
a letter
```

## Output sample:
Print capitalized words in the following way.
```
Hello World
JavaScript Language
A Letter
```
-}

import System.Environment
import Data.Char (toUpper)

capitalize :: String -> String
capitalize s = unwords $ map capitalizeHelper ws
    where capitalizeHelper w = (toUpper (head w)) : (tail w)
          ws = words s

main = do 
    args <- getArgs
    let fileName = head args
    contents <- readFile fileName
    let inputs = lines contents
    let outputs = map capitalize inputs
    mapM putStrLn outputs
