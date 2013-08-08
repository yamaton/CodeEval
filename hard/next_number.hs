{-
Following Integer
==================
Description
------------
Credits: This challenge has appeared in a past google competition

You are writing out a list of numbers. Your list contains all numbers with exactly Di digits in its decimal representation which are equal to i, for each i between 1 and 9, inclusive. You are writing them out in ascending order. For example, you might be writing every number with two '1's and one '5'. Your list would begin 115, 151, 511, 1015, 1051. Given N, the last number you wrote, compute what the next number in the list will be. The number of 1s, 2s, ..., 9s is fixed but the number of 0s is arbitrary.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 10^6

Output sample
--------------
For each line of input, generate a line of output which is the next integer in the list.
-}

import System.Environment (getArgs)
import Data.List (sort)

integerDigits :: Int -> [Int]
integerDigits n = map (read . (:[])) (show n)

fromDigits :: [Int] -> Int
fromDigits xs = read $ concatMap show xs


---- this is mindless translation from python code
nextLexicographicPermutation :: Ord a => [a] -> [a]
nextLexicographicPermutation xs
  | null ks    = []
  | otherwise = upper ++ reverse lower
    where
      ks = filter (\i -> xs !! i < xs !! (i + 1)) [0 .. length xs - 2]
      k = last ks
      l = last $ filter (\j -> xs !! k < xs !! j) [k + 1 .. length xs - 1]
      ys = first ++ [xs !! l] ++ second ++ [xs !! k] ++ third
        where (former, _:third) = splitAt l xs
              (first, _:second) = splitAt k former
      (upper, lower) = splitAt (k+1) ys


nextNumber :: Int -> Int
nextNumber n
  | null nextDigits = fromDigits $ [y] ++ replicate (zeroCount + 1) 0 ++ ys
  | otherwise       = fromDigits nextDigits
    where xs = integerDigits n
          nextDigits = nextLexicographicPermutation xs 
          y:ys = sort $ filter (> 0) xs
          zeroCount = length $ filter (== 0) xs

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map read $ lines contents
  let outputs = map nextNumber inputs
  mapM_ print outputs

