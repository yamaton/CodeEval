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
capitalize s = unwords $ map capitalizeHelper $ words s
  where capitalizeHelper (w:ws) = toUpper w : ws

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = lines contents
  let outputs = map capitalize inputs
  mapM_ putStrLn outputs
