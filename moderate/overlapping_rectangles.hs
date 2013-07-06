{-
Description
-----------
Given two axis aligned rectangles A and B, determine if the two overlap.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains 8 comma separated co-ordinates. The co-ordinates are upper left x of A, upper left y of A, lower right x of A, lower right y of A, upper left x of B, upper left y of B, lower right x of B, lower right y of B. e.g.
```
-3,3,-1,1,1,-1,3,-3
-3,3,-1,1,-2,4,2,2
```

Output sample
--------------
Print out True or False if A and B intersect. e.g.
```
False
True
```
-}

import System.Environment (getArgs)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : split c s'' where (w, s'') = break (== c) s'

ifRectanglesIntersect :: [Int] -> Bool
ifRectanglesIntersect xs = condA && condB
    where [x1min, y1max, x1max, y1min, x2min, y2max, x2max, y2min] = xs
          condA = (x2min - x1max) * (x2max - x1min) < 0
          condB = (y2min - y1max) * (y2max - y1min) < 0

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let inputs = [map read (split ',' line) | line <- lines contents]
    let outputs = map ifRectanglesIntersect inputs
    mapM print outputs

