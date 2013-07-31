{-
Prefix Expressions
===================
Updated by Yamato Matsuoka on 2013-07-23
Created by Yamato Matsuoka on 2013-07-12

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

[Comment]
I had to transform Int from/to String back and forth such that I can deal with [*, +, 2, 3, 4] as a stack.
Better data structure needed to remove these unnecessary conversions.

-}

import System.Environment (getArgs)

evaluate :: [String] -> Int
evaluate xs = head $ foldl helper [] (reverse xs) where
  helper :: [Int] -> String -> [Int]
  helper (x:y:ys) "+" = (x + y) : ys
  helper (x:y:ys) "-" = (x - y) : ys
  helper (x:y:ys) "*" = (x * y) : ys
  helper (x:y:ys) "/" = (x `div` y) : ys
  helper xs numString = read numString : xs

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map words $ lines contents
  let outputs = map evaluate inputs
  mapM_ print outputs
