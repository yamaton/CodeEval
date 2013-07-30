{-
find_square.hs

Created by Yamato Matsuoka on 2013-07-03.

Find a Square
==============

Challenge Description
----------------------
You have coordinates of four points on a plane. Check whether they make a square.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Input example is the following
```
(1,6), (6,7), (2,7), (9,1)
(4,1), (3,4), (0,5), (1,2)
(4,6), (5,5), (5,6), (4,5)
```
All numbers in input are integers between 0 and 10

Output sample
-------------
Print results in the following way.
```
false
false
true
```
-}

import System.Environment (getArgs)
import Data.List (sort)

type Point = (Int, Int)

boolToString :: Bool -> String
boolToString True  = "true"
boolToString False = "false"

parser :: String -> [Point]
parser s = read $ '[' : s ++ "]"

isSquare :: [Point] -> Bool
isSquare xs = all (== minimum dists) $ take 4 (sort dists)
                where [p1, p2, p3, p4] = xs
                      d1 = distanceSquared p1 p2
                      d2 = distanceSquared p3 p4 
                      d3 = distanceSquared p1 p3
                      d4 = distanceSquared p2 p4
                      d5 = distanceSquared p1 p4
                      d6 = distanceSquared p2 p3
                      dists = [d1, d2, d3, d4, d5, d6]

distanceSquared :: Point -> Point -> Int
distanceSquared (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map parser $ lines contents
    let outputs = map (boolToString . isSquare) inputs
    mapM putStrLn outputs
