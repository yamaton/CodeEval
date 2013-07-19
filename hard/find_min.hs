{-
Description
------------
Credits: This problem appeared in the Facebook Hacker Cup 2013 Hackathon.

After sending smileys, John decided to play with arrays. Did you know that hackers enjoy playing with arrays? John has a zero-based index array, m, which contains n non-negative integers. However, only the first k values of the array are known to him, and he wants to figure out the rest.

John knows the following: for each index i, where k <= i < n, m[i] is the minimum non-negative integer which is *not* contained in the previous *k* values of m.

For example, if k = 3, n = 4 and the known values of m are [2, 3, 0], he can figure out that m[3] = 1. John is very busy making the world more open and connected, as such, he doesn't have time to figure out the rest of the array. It is your task to help him. Given the first k values of m, calculate the nth value of this array. (i.e. m[n - 1]). Because the values of n and k can be very large, we use a pseudo-random number generator to calculate the first k values of m. Given positive integers a, b, c and r, the known values of m can be calculated as follows:

m[0] = a
m[i] = (b * m[i - 1] + c) % r, 0 < i < k

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains 6 comma separated positive integers which are the values of n,k,a,b,c,r in order. e.g.
```
97,39,34,37,656,97
186,75,68,16,539,186
137,49,48,17,461,137
98,59,6,30,524,98
46,18,7,11,9,46
```

Output sample
--------------
Print out the nth element of m for each test case e.g.
```
8
38
41
40
12
```
-}

import System.Envionment (getArgs)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


psudoRandomSeq :: Int -> Int -> Int -> Int -> [Int]
psudoRandomSeq a b c r = iterate (\m -> (b * m + c) `mod` r) a


reader :: String -> [Int]
reader s = map read $ split ',' s


findMin :: [Int] -> Int
findMin xs = 
  where
    [n k a b c r] = xs


main = do
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map reader $ lines contents
    let outputs = map findMin inputs
    mapM_ print outputs
