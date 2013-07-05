{-
dec2bin.hs

Created by Yamato Matsuoka on 2013-07-04.

Description
------------
Given a decimal (base 10) number, print out its binary representation.

Input sample
-------------
File containing positive whole decimal numbers, one per line. e.g. 
```
2
10
67
```

Output sample
--------------
Print the decimal representation, one per line. e.g.
```
10
1010
1000011
```

-}

import System.Environment (getArgs)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

decToBin :: Int -> String
decToBin n = showIntAtBase 2 intToDigit n ""

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs  = map read $ lines contents
    let outputs = map decToBin inputs
    mapM putStrLn outputs



