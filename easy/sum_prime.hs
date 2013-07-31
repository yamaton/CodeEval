{-
Sum of Primes
==============
Created by Yamato Matsuoka on 2013-07-03.

Description
-----------
Write a program to determine the sum of the first 1000 prime numbers.

Input sample
------------
None

Output sample
--------------
Your program should print the sum on stdout.i.e.

-}

import Control.Monad (when, forM_)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, assocs)


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


-- Rosser's theorem is used to get an upper bound:
-- For n-th prime number P(n), for n > 6
-- log(n) + log(log(n)) - 1 < P(n)/n < log(n) + log(log(n))  
-- http://en.wikipedia.org/wiki/Prime_number_theorem     
primes :: Int -> [Int]
primes n
  | n < 6     = take n [2, 3, 5, 7, 11]
  | otherwise = take n $ [i | (i,True) <- assocs $ sieve ub]
    where 
      x = fromIntegral n
      ub = floor $ x * (log x + log (log x))

main = (print . sum . primes) 1000

