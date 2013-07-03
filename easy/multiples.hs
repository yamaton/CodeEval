{-
multiples.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
Given numbers x and n, where n is a power of 2, print out the smallest multiple of n which is greater than or equal to x. Do not use division or modulo operator.

Input sample
-------------
The first argument will be a text file containing a comma separated list of two integers, one list per line. e.g. 
```
13,8
17,16
```

Output sample
--------------
Print to stdout, the smallest multiple of n which is greater than or equal to x, one per line. e.g.
```
16
32
```
-}
import System.Environment (getArgs)
import Data.List (find)
import Data.Maybe (fromJust)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s''
        where (w, s'') = break (== c) s'

multiple :: Int -> Int -> Int
multiple x n = fromJust $ find (>= x) [n, 2*n ..]

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [map read (split ',' s)| s <- lines contents]
    let outputs = [multiple x n | [x, n] <- inputs]
    mapM print outputs