{-
Counting Primes
================
Created by Yamato Matsuoka on 2013-07-04

Description
------------
Given two integers N and M, count the number of prime numbers between N and M (both inclusive)

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated positive integers. e.g.
```
2,10
20,30
```

Output sample
--------------
Print out the number of primes between N and M (both inclusive)

-}

import System.Environment (getArgs)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : split c s''
                    where (w, s'') = break (== c) s'

-- | Eratosthenes Sieve
-- >>> primesTo 20
-- [2,3,5,7,11,13,17,19]
primesTo :: Int -> [Bool]
primesTo 2    = [2]
primesTo n = 
  | n < 2     = []
  | otherwise = flip evalState initial $ do
      forM 
  
  
    initial = (take (n + 1) $ repeat True) :: [Bool]
    maxP = floor . sqrt $ fromIntegral n




primePi :: Int -> Int
primePi = length . primesTo

countPrimes :: Int -> Int -> Int
countPrimes a b
  | a > b     = 0
  | a < 2     = primePi b
  | otherwise = primePi b - (primePi (a - 1))


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = [map read (split ',' line) | line <- lines contents]
    let outputs = [countPrimes a b | [a, b] <- inputs ]
    mapM_ print outputs

