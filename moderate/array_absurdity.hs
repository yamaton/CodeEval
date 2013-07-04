{-
array_absurdity.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
Imagine we have an immutable array of size N which we know to be filled with integers ranging from 0 to N-2, inclusive. Suppose we know that the array contains exactly one duplicated entry and that duplicate appears exactly twice. Find the duplicated entry. (For bonus points, ensure your solution has constant space and time proportional to N)

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Ignore all empty lines. Each line begins with a positive integer(N) i.e. the size of the array, then a semicolon followed by a comma separated list of positive numbers ranging from 0 to N-2, inclusive. i.e eg.
```
5;0,1,2,3,0
20;0,1,10,3,2,4,5,7,6,8,11,9,15,12,13,4,16,18,17,14
```

Output sample
--------------
Print out the duplicated entry, each one on a new line eg
```
0
4
```

-}

import System.Environment (getArgs)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : split c s''
                    where (w, s'') = break (== c) s'

parser :: String -> (Int, [Int])
parser s = (read former, [read n | n <- split ',' latter])
    where [former, latter] = split ';' s

findDuplicate :: Int -> [Int] -> Int
findDuplicate n xs = (n-1) - ((n * (n-1) `div` 2) - sum xs) 

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = map parser $ filter (not . null) $ lines contents
    let outputs = [findDuplicate n xs | (n, xs) <- inputs]
    mapM print outputs




