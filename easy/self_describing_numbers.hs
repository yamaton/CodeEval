{-
self_describing_numbers.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
A number is a self-describing number when (assuming digit positions are labeled 0 to N-1), the digit in each position is equal to the number of times that that digit appears in the number.

Input sample
-------------
The first argument is the pathname to a file which contains test data, one test case per line. Each line contains a positive integer. Each line is in the format: N i.e. a positive integer eg.
```
2020
22
1210
```

Output sample
-------------
If the number is a self-describing number, print out a 1. If not, print out a 0 eg.
```
1
0
1
```

For the curious, here's how 2020 is a self-describing number: Position '0' has value 2 and there is two 0 in the number. Position '1' has value 0 because there are not 1's in the number. Position '2' has value 2 and there is two 2. And the position '3' has value 0 and there are zero 3's.

-}

import System.Environment (getArgs)

count :: Int -> [Int] -> Int
count x xs = length $ filter (== x) xs

integerDigits :: Int -> [Int]
integerDigits n = reverse . map (`mod` 10) $ takeWhile (> 0) $ iterate (`div` 10) n

isSelfDescribing :: Int -> Bool
isSelfDescribing n = xs == ys
            where xs = integerDigits n
                  ys = map (\x -> count x xs) [0 .. (length xs - 1)]

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = map read $ lines contents
    let outputs = map (boolToInt . isSelfDescribing) inputs
    mapM print outputs