{-
Flavius Josephus
================
Description
-----------
Flavius Josephus was a famous Jewish historian of the first century, at the time of the destruction of the Second Temple. According to legend, during the Jewish-Roman war he was trapped in a cave with a group of soldiers surrounded by Romans. Preferring death to capture, the Jews decided to form a circle and, proceeding around it, to kill every j'th person remaining until no one was left. Josephus found the safe spot in the circle and thus stayed alive. Write a program that returns a list of n people, numbered from 0 to n-1, in the order in which they are executed.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated positive integers n and m , where n is the number of people and every m'th person will be executed. e.g.
```
10,3
5,2
```

Output sample
--------------
Print out the list of n people(space delimited) in the order in which they will be executed. e.g.
```
2 5 8 1 6 0 7 4 9 3
1 3 0 4 2
```
-}

import System.Environment (getArgs)
import Data.List (delete)

flavius :: Int -> Int -> [Int]
flavius n step = flaviusHelper step 0 [0 .. (n-1)] 


flaviusHelper :: Int -> Int -> [Int] -> [Int]
flaviusHelper  _   _   []      = []
flaviusHelper step ptr people = killed : flaviusHelper step nextPtr (delete killed people)
  where
    currIdx = length $ takeWhile (/= ptr) people
    killed  = last $ take (currIdx + step) $ cycle people
    nextPtr = last $ take (currIdx + step + 1) $ cycle people


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = [map read (split ',' line) | line <- lines contents]
  let outputs = [flavius n steps | [n, steps] <- inputs]
  mapM_ putStrLn [unwords (map show out) | out <- outputs]
