{-
nrc.hs

Created by Yamato Matsuoka on 2013-07-06.

Description
------------
Write a program to find the first non repeated character in a string.

Input sample
-------------
The first argument will be a text file containing strings. e.g. 
```
yellow
tooth
```

Output sample
--------------
Print to stdout, the first non repeating character, one per line. e.g.
```
y
h
```
-}

import System.Environment (getArgs)

nonRepeatedChar :: String -> Char
nonRepeatedChar s = head [c | c <- s, length (filter (== c) s) == 1]

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = lines contents
    let outputs = map nonRepeatedChar inputs
    mapM putStrLn $ map (\c -> c:[]) outputs

