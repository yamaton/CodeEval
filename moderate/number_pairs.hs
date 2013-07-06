{-
number_pairs.hs

Created by Yamato Matsuoka on 2013-07-06.

Description
------------
You are given a sorted array of positive integers and a number 'X'. Print out all pairs of numbers whose sum is equal to X. Print out only unique pairs and the pairs should be in ascending order

Input sample
-------------
Your program should accept as its first argument a filename. This file will contain a comma separated list of sorted numbers and then the sum 'X', separated by semicolon. Ignore all empty lines. If no pair exists, print the string NULL eg.
```
1,2,3,4,6;5
2,4,5,6,9,11,15;20
1,2,3,4;50
```

Output sample
--------------
Print out the pairs of numbers that equal to the sum X. The pairs should themselves be printed in sorted order i.e the first number of each pair should be in ascending order .e.g.
```
1,4;2,3
5,15;9,11
NULL
```
-}

import System.Environment (getArgs)
import Data.List (intercalate)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s'' where (w, s'') = break (== c) s'

join :: Char -> [String] -> String
join c s = intercalate (c:[]) s

findPairs :: Int -> [Int] -> [(Int, Int)]
findPairs n xs = out
    where nHalf = n `div` 2
          bag1 = filter (<= nHalf) xs
          bag2 = filter (> nHalf) xs
          bag3 = filter (== nHalf) xs
          pairs = [(i, j) |  i <- bag1, j <- bag2, i + j  == n]
          out = if ((length bag3 > 1) && (nHalf * 2 == n)) 
                    then pairs ++ [(nHalf, nHalf)]
                    else pairs

parser :: String -> ([Int], Int)
parser s = (numbers, sumValue)
    where [former, latter] = split ';' s
          sumValue = read latter
          numbers = map read $ split ',' former

format :: [(Int, Int)] -> String
format [] = "NULL"
format xs = join ';' (map (tail . init . show) xs)

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = map parser $ lines contents
    let outputs = [findPairs n xs | (xs, n) <- inputs]
    mapM putStrLn $ map format outputs

