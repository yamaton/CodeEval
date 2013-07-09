{-
Created by Yamato Matsuoka on 2013-07-08

Reverse Groups
===============
Description
------------
Given a list of numbers and a positive integer k, reverse the elements of the list, k items at a time. If the number of elements is not a multiple of k, then the remaining items in the end should be left as is.

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a list of numbers and the number k, separated by a semicolon. The list of numbers are comma delimited. e.g.
```
1,2,3,4,5;2
1,2,3,4,5;3
```

Output sample
--------------
Print out the new comma separated list of numbers obtained after reversing. e.g.
```
2,1,4,3,5
3,2,1,4,5
```
-}

import System.Environment (getArgs)
import Data.List (intercalate)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = map (take n) $ takeWhile (not . null) $ iterate (drop n) xs

reverseEvery :: Int -> [a] -> [a]
reverseEvery n xs = concat $ map reverse (splitEvery n xs)

reverseGroup :: [Int] -> Int -> [Int]
reverseGroup xs k = xss ++ remainder
    where (body, remainder) = splitAt (((length xs) `div` k) * k) xs
          xss = reverseEvery k body

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : split c s'' 
                     where (w, s'') = break (== c) s'

join :: Char -> [String] -> String
join c s = intercalate (c:[]) s

parser :: String -> ([Int], Int)
parser s = (map read (split ','  former), read latter)
    where [former, latter] = split ';' s 


main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = map parser $ lines contents
    let outputs = [reverseGroup xs n | (xs, n) <- inputs]
    mapM putStrLn [join ',' (map show out) | out <- outputs]

