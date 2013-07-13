{-
stack.hs

Created by Yamato Matsuoka on 2013-07-09.

Description
------------
Write a program implementing a stack inteface for integers. The interface should have 'push' and 'pop' functions. You will be asked to 'push' a series of integers and then 'pop' and print out every alternate integer.

Input sample
-------------
The first argument will be a text file containing a series of space delimited integers, one per line. e.g. 
```
1 2 3 4
10 -2 3 4
```

Output sample
--------------
Print to stdout, every alternate integer(space delimited), one per line. e.g.
```
4 2
4 -2
```
-}

import System.Environment (getArgs)

push :: [Int] -> Int -> [Int]
push xs n = n : xs

pushAndPop :: [Int] -> [Int]
pushAndPop [] = []
pushAndPop xs = s : (takeEvery 2 stack)
  where 
    (s:stack) = foldl push [] xs

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys 

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = [map read (words line) | line <- lines contents]
    let outputs = map pushAndPop inputs
    mapM putStrLn [unwords (map show out) | out <- outputs]
