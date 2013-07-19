{-
prefix.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
------------
You are given a prefix expression. Write a program to evaluate it.

Input sample
-------------
The first argument will be an input file with one prefix expression per line. e.g.
```
* + 2 3 4
```
Your program has to read this and insert it into any data structure you like. Traverse that data structure and evaluate the prefix expression. Each token is delimited by a whitespace. You may assume that the only valid operators appearing in test data are '+','*' and '/' 

Output sample
-------------
Print to stdout, the output of the prefix expression, one per line. e.g.
```
20
```

-}

import System.Environment (getArgs)


interpret :: => Char -> (Int -> Int -> Int)
interpret '+' = (+)
interpret '*' = (*)
interpret '/' = div

evaluate :: [String] -> Int
evaluate xs = undefined

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map words $ lines read
  let outputs = map evaluate inputs
  mapM_ print outputs

