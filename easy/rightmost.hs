{-
rightmost.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
You are given a string 'S' and a character 't'. Print out the position of the rightmost occurrence of 't'(case matters) in 'S' or -1 if there is none. The position to be printed out is zero based.

Input sample
-------------
The first argument is a file, containing a string and a character, comma delimited, one per line. Ignore all empty lines in the input file.e.g. 
```
Hello World,r
Hello CodeEval,E
```

Output sample
--------------
Print out the zero based position of the character 't' in string 'S', one per line. Do NOT print out empty lines between your output. e.g.
```
8
10
```

-}

import System.Environment (getArgs)
import Data.List (findIndices)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s''
        where (w, s'') = break (== c) s'

parser :: String -> (String, Char)
parser s = let [s1, c:[]] = split ',' s
           in (s1, c)


getLastIndex :: Char -> String -> Int
getLastIndex c s = if (null xs) then (-1) else (last xs)
                       where xs = findIndices (== c) s

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = map parser $ lines contents
    let outputs = [getLastIndex c s| (s, c) <- inputs]
    mapM print outputs


    
