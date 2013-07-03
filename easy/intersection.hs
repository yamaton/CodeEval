{-
intersection.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
You are given two sorted list of numbers(ascending order). The lists themselves are comma delimited and the two lists are semicolon delimited. Print out the intersection of these two sets.

Input sample
-------------
File containing two lists of ascending order sorted integers, comma delimited, one per line. e.g. 
```
1,2,3,4;4,5,6
7,8,9;8,9,10,11,12
```

Output sample
--------------
Print out the ascending order sorted intersection of the two lists, one per line
e.g.
```
4
8,9
```
-}

import System.Environment (getArgs)
import Data.List (intercalate, intersect)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : split c s''
                    where (w, s'') = break (== c) s'

join :: Char -> [Int] -> String
join c xs = intercalate (c:[]) $ map show xs


parseLine :: String -> ([Int], [Int])
parseLine s = (map read former, map read latter) 
    where [former, latter] = (map (split ',') $ split ';' s)  :: [[String]]

main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = map parseLine $ lines contents
    let outputs = [intersect xs ys | (xs, ys) <- inputs]
    mapM putStrLn $ map (join ',') outputs

