{-
str_perm.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
------------
Write a program to print out all the permutations of a string in alphabetical order.

Input sample
------------
The first argument will be a text file containing an input string, one per line. e.g. 
```
hat
```

Output sample
--------------
Print to stdout, permutations of the string, comma separated, in alphabetical order. e.g.
```
aht,ath,hat,hta,tah,tha
```

-}

import System.Environment (getArgs)
import Data.List (permutations, sort, intercalate, nub)

strPerm :: String -> [String]
strPerm = sort . nub . permutations 

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = lines contents
    let outputs = map strPerm inputs
    mapM_ (putStrLn . (intercalate ",")) outputs

