{-
position.hs

Created by Yamato Matsuoka on 2013-07-03.

## Description
Given a number n and two integers p1, p2 determine if the bits in position p1 and p2 are the same or not. Positions p1, p2 and 1 based.

## Input sample
The first argument will be a text file containing a comma separated list of 3 integers, one list per line. e.g. 
```
86,2,3
125,1,2
```

## Output sample
Print to stdout, 'true'(lowercase) if the bits are the same, else 'false'(lowercase). e.g.
```
true
false
```
Note: 86 = "1010110" (in base 2)
     125 = "1111101" (in base 2)
-}

import System.Environment (getArgs)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s''
        where (w, s'') = break (== c) s'

myFormat :: Bool -> String
myFormat True = "true"
myFormat False = "false"

myPosition :: Int -> Int -> Int -> Bool
myPosition n p1 p2 = let binExprReversed = reverse $ showIntAtBase 2 intToDigit n ""
                     in binExprReversed !! (p1-1) == binExprReversed !! (p2-1)

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [[read s | s <- split ',' line] | line <- lines contents]
    let outputs = [myFormat (myPosition n p1 p2) | [n, p1, p2] <- inputs]
    mapM putStrLn outputs
