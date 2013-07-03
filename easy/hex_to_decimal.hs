{-
Hex to Decimal

Created by Yamato Matsuoka on 2013-07-03

Description
------------
You will be given a hexadecimal(base 16) number. Convert it into decimal (base 10)

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a hex number. You may assume that the hex number does not have the leading 'Ox'. Also all alpha characters(e.g. a through f) in the input will be in lowercase e.g.
```
9f
11
```

Output sample
--------------
Print out the equivalent decimal number e.g.
```
159
17
```
-}

import System.Environment (getArgs)
import Numeric (readHex)

hexToDec :: String -> Int
hexToDec s = fst.head $ readHex s

main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = lines contents
    let outputs = map hexToDec inputs
    mapM print outputs
    