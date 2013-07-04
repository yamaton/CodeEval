{-
sumint.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
Print out the sum of integers read from a file.

Input sample
------------
The first argument to the program will be a text file containing a positive integer, one per line. e.g. 
```
5
12
```
NOTE: For solutions in JavaScript, assume that there are 7 lines of input

Output sample
-------------
Print out the sum of all the integers read from the file.  e.g.
```
17
```
-}

import System.Environment (getArgs)

main = do 
    args <- getArgs
    contents <- readFile $ head args
    let output  = sum $ map read $ lines contents
    print output
