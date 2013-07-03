{-
nmodm.hs
Created by Yamato on 2013-07-03

N Mod M
========

Description
------------
Given two integers N and M, calculate N Mod M (without using any inbuilt modulus operator).

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated positive integers. e.g.
```
20,6
2,3
```
You may assume M will never be zero.

Output sample
--------------
Print out the value of N Mod M

-}
import System.Environment (getArgs)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s''
        where (w, s'') = break (== c) s'

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [[read s | s <- split ',' line] | line <- lines contents]
    let outputs = [n `mod` m | [n, m] <- inputs]
    mapM print outputs
