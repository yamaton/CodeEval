{-
prime_palindrome.hs

Created by Yamato Matsuoka on 2012-07-03.

## Description
Write a program to determine the biggest prime palindrome under 1000.

## Input
None

## Output
Prints the largest palindrome on stdout under 1000.
-}

isPalindrome :: Int -> Bool
isPalindrome n = let s = show n
                 in (s == reverse s)

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
    | even n    = False
    | otherwise = let ub = (floor . sqrt . fromIntegral) n
                  in all (\p -> mod n p /= 0) [3, 5 .. ub]

main = print out
    where out = head $ filter isPrime $ filter isPalindrome [1000, 999 ..]