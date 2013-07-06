{-
m2last.hs

Created by Yamato Matsuoka on 2013-07-06.

Description
------------
Write a program to determine the Mth to last element of a list.

Input sample
-------------
The first argument will be a text file containing a series of space delimited characters followed by an integer representing a index into the list(1 based), one per line. e.g. 
```
a b c d 4
e f g h 2
```

Output sample
--------------
Print to stdout, the Mth element from the end of the list, one per line. If the index is larger than the list size, ignore that input. e.g.
```
a
g
```
-}

import System.Environment (getArgs)

takeLast :: Int -> [String] -> String
takeLast n xs 
    | n > length xs = []
    | otherwise     = (reverse xs) !! (n-1)

parser :: String -> ([String], Int)
parser s = (s', n)
    where xs = words s
          n  = read (last xs)
          s' = init xs

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = map parser $ lines contents
    let outputs = filter (not . null) [takeLast n s | (s, n) <- inputs]
    mapM putStrLn outputs
