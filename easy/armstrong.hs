{-
 armstrong.hs

Armstrong Number
=================

Description
------------
An Armstrong number is an n-digit number that is equal to the sum of the n'th powers of its digits. 
Determine if the input numbers are Armstrong numbers.

Input sample
-------------
Your program should accept as its first argument a path to a filename. 
Each line in this file has a positive integer. e.g.
```
6
153
351
```

Output sample
--------------
Print out True/False if the number is an Armstrong number or not e.g.
```
True
True
False
```
-}

import System.Environment (getArgs)

integerDigits :: Int -> [Int]
integerDigits n = reverse . map (`mod` 10) $ takeWhile (> 0) $ iterate (`div` 10) n

isArmstrong :: Int -> Bool
isArmstrong n = 
    let len = length digits
    in n == sum (map (^ len) digits)
        where digits = integerDigits n

main = do
    args <- getArgs
    let fileName = head args
    contents <- readFile fileName
    let inputs = map read $ lines contents
    let outputs = map isArmstrong inputs
    mapM print outputs
