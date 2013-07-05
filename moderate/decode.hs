{-
Decode Numbers
==============

Created by Yamato Matsuoka on 2013-07-04

Description
------------
You are given an encoded message containing only numbers. You are also provided with the following mapping:

A : 1
B : 2
C : 3
...
Z : 26
Given an encoded message, count the number of ways it can be decoded.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is a testcase and contains an encoded message of numbers. e.g.
```
12
123
```
You may assume that the test cases contain only numbers.

Output sample
--------------
Print out the different number of ways it can be decoded. e.g.
```
2
3
```
-}
import System.Environment (getArgs)

decode :: String -> Int
decode s
    | length s == 0 = 1
    | length s == 1 = 1
    | otherwise     = if (0 < x && x < 26)
                          then (decode (drop 1 s)) + (decode (drop 2 s))
                          else (decode (drop 1 s))
                        where x = read (take 2 s)

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = lines contents
    let outputs = map decode inputs
    mapM print outputs

