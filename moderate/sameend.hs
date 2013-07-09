{-
sameend.hs

Created by Yamato Matsuoka on 2012-07-09.

Description
------------
You are given two strings 'A' and 'B'. Print out a 1 if string 'B' occurs at the end of string 'A'. Else a zero.

Input sample
-------------
The first argument is a file, containing two strings, comma delimited, one per line. Ignore all empty lines in the input file.e.g. 
```
Hello World,World
Hello CodeEval,CodeEval
San Francisco,San Jose
```

Output sample
--------------
Print out 1 if the second string occurs at the end of the first string. Else zero. Do NOT print out empty lines between your output. e.g.
```
1
1
0
```
-}

import System.Environment (getArgs)

isSameEnd :: String -> String -> Bool
isSameEnd former latter
    | len1 < len2 = False
    | otherwise   = drop (len1 - len2) former == latter
       where 
         len1 = length former
         len2 = length latter

boolToInt :: Bool -> Int 
boolToInt True  = 1
boolToInt False = 0

parser :: String -> (String, String)
parser s = (former, tail latter)
    where (former, latter) = break (== ',') s

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = map parser $ filter (not . null) $ lines contents
    let outputs = [boolToInt (isSameEnd s1 s2) | (s1, s2) <- inputs]
    mapM print outputs

