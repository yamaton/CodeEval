{-
String Rotation
================
Description
------------
You are given two strings. Determine if the second string is a rotation of the first string.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated strings. e.g.
```
Hello,lloHe
Basefont,tBasefon
```

Output sample
--------------
Print out True/False if the second string is a rotation of the first. e.g.
```
True
True
```
-}

import System.Environment (getArgs)

isStringRotation :: String -> String -> Bool
isStringRotation s t
    | lenS == lenT = t `elem` [latter ++ former | (former, latter) <- map (`splitAt` s) [0 .. lenS]]
    | otherwise    = False
        where 
            lenS = length s
            lenT = length t

parser :: String -> (String, String)
parser s = (former, tail latter)
    where (former, latter) = break (== ',') s


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map parser $ lines contents
    let outputs = [isStringRotation s1 s2 | (s1, s2) <- inputs]
    mapM_ print outputs
