{-
String List
============
Created by Yamato Matsuoka on 2013-07-12.

Description
------------
Credits: Challenge contributed by Max Demian.

You are given a number N and a string S. Print all of the possible ways to write a string of length N from the characters in string S, comma delimited in alphabetical order.


Input sample
-------------
The first argument will be the path to the input filename containing the test data. Each line in this file is a separate test case. Each line is in the format: N,S i.e. a positive integer, followed by a string (comma separated) eg.
```
1,aa
2,ab
3,pop
```

Output sample
--------------
Print all of the possible ways to write a string of length N from the characters in string S comma delimited in alphabetical order, with no duplicates. eg.
```
a
aa,ab,ba,bb
ooo,oop,opo,opp,poo,pop,ppo,ppp
```
-}

import System.Environment (getArgs)
import Control.Monad (replicateM)
import Data.List (intercalate, nub, sort)

combinationsWithReplacement :: Ord a => Int -> [a] -> [[a]]
combinationsWithReplacement n xs = sort . nub $ replicateM n xs

reader :: String -> (Int, String)
reader s = (read former, latter)
    where (former, _:latter) = break (== ',') s

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map reader $ filter (not . null) $ lines contents
    let outputs = [combinationsWithReplacement n xs | (n, xs) <- inputs]
    mapM_ (putStrLn . (intercalate ",")) outputs
