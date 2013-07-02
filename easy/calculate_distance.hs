{-
Challenge Description
======================
You have coordinates of 2 points and need to find the distance between them.

## Input sample
Your program should accept as its first argument a path to a filename. Input example is the following
```
(25, 4) (1, -6)

```
All numbers in input are integers between -100 and 100.

## Output sample
Print results in the following way.
```
26
90
```
You don't need to round the results you receive. 
They must be integer numbers between -100 and 100.
-}

import System.Environment
import Data.Char (isDigit)

distance :: [Int] -> Int
distance xs = round $ sqrt $ realToFrac ((x2 - x1)^2 + (y2 - y1)^2) 
    where [x1, y1, x2, y2] = xs

myReader :: String -> [Int]
myReader s = map read $ words [c | c <- s, isDigit c || c `elem` " -"]

main = do
    args <- getArgs
    let fileName = head args
    contents <- readFile fileName
    let inputs = map myReader $ lines contents
    let outputs = map distance inputs
    mapM print outputs