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
import Control.Monad (when, forM_)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, assocs)


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


---- Eratosthenes sieve
---- it can be more efficient by treating odd numbers only but it's good enough.
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  let maxP = floor . sqrt $ fromIntegral n
  sieveTF <- newArray (2, n) True 
  forM_ [2..maxP] $ \p -> do
    isPrime <- readArray sieveTF p
    when isPrime $ do
      forM_ [p*p, p*p+p .. n] $ \q -> do
        writeArray sieveTF q False
  return sieveTF


primePi :: Int -> Int
primePi n
  | n < 2     = 0
  | otherwise = length [i | (i,True) <- assocs $ sieve n]


countPrimes :: Int -> Int -> Int
countPrimes a b = primePi b - primePi (a - 1)


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = [map read (split ',' line) | line <- lines contents]
  let outputs = [countPrimes a b | [a, b] <- inputs ]
  mapM_ print outputs
