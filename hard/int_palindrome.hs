{-
int_palindrome.py

Created: 2013-07-03 by Yamato Matsuoka

Palindromic Ranges
===================

Description:
------------
A positive integer is a palindrome if its decimal representation (without leading zeros) is a palindromic string (a string that reads the same forwards and backwards). For example, the numbers 5, 77, 363, 4884, 11111, 12121 and 349943 are palindromes.

A range of integers is interesting if it contains an even number of palindromes. The range [L, R], with L <= R, is defined as the sequence of integers from L to R (inclusive): (L, L+1, L+2, ..., R-1, R). L and R are the range's first and last numbers.

The range [L1,R1] is a subrange of [L,R] if L <= L1 <= R1 <= R. Your job is to determine how many interesting subranges of [L,R] there are.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain two positive integers, L and R (in that order), separated by a space. eg.
```
1 2
1 7
```

Output sample
--------------
For each line of input, print out the number of interesting subranges of [L,R] eg.
```
1
12
```
-}

import System.Environment (getArgs)

isPalindromic :: Int -> Bool
isPalindromic n = let s = show n
                  in (s == reverse s)

isInteresting :: [Int] -> Bool
isInteresting xs = even ( length $ filter isPalindromic xs )

slice :: Int -> Int -> [a] -> [a]
slice from to xs = drop from (take to xs)

subRanges :: [Int] -> [[Int]]
subRanges xs = let n = length xs
               in [slice from to xs | from <- [0 .. (n-1)], to <- [(from + 1) .. n]]
    
numInterestingSubranges :: Int -> Int -> Int
numInterestingSubranges p q = length $ filter isInteresting (subRanges [p .. q])

main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [[read w | w <- (words line)] | line <- lines contents] :: [[Int]]
    let outputs = [numInterestingSubranges p q | [p, q] <- inputs]
    mapM print outputs

