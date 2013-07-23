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


primes :: [Int]

main = do 
    print $ sum (take 1000 primes)

