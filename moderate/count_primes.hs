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

sieve :: Int -> [Int]
sieve n 
    | n < 2     = []
    | otherwise = 
        where maxP = round $ sqrt (fromIntegral n)
            numbers = 

primePi :: Int -> Int
primePi = length . sieve

countPrimes :: Int -> Int -> Int
countPrimes a b = (primePi b) - (primePi (a - 1))


def EratosthenesSieve(N):
    """Construct a list of primes equal or less than N."""
    if N < 2:
        return []
    numbers = [True] * (N + 1)
    max_p = int(math.sqrt(N))
    for p in (i for i in range(2, max_p+1) if numbers[i]):
        for q in range(p*p, N+1, p):
            numbers[q] = False
    return [i for i in range(2, N+1) if numbers[i]]


main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [map read (split ',' line) | line <- lines contents]
    let outputs = [ countPrimes a b | [a, b] <- inputs ]

