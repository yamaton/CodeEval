{-
closest_pair.hs

Created by Yamato Matsuoka on 2013-07-10.

Description
------------
Credits: Programming Challenges by Steven S. Skiena and Miguel A. Revilla

You will be given the x/y co-ordinates of several locations. You will be laying out 1 cable between two of these locations. In order to minimise the cost, your task is to find the shortest distance between a pair of locations, so that pair can be chosen for the cable installation.

Input sample
-------------
Your program should accept as its first argument a path to a filename.The input file contains several sets of input. Each set of input starts with an integer N (0<=N<=10000), which denotes the number of points in this set. The next N line contains the coordinates of N two-dimensional points. The first of the two numbers denotes the X-coordinate and the latter denotes the Y-coordinate. The input is terminated by a set whose N=0. This set should not be processed. The value of the coordinates will be less than 40000 and non-negative. eg.
```
5
0 2
6 67
43 71
39 107
189 140
0
```

Output sample
--------------
For each set of input produce a single line of output containing a floating point number (with four digits after the decimal point) which denotes the distance between the closest two points. If there is no such two points in the input whose distance is less than 10000, print the line INFINITY. eg.
```
36.2215
```

[Comment]
This is closest pair of points problem
http://en.wikipedia.org/wiki/Closest_pair_of_points_problem
-}

import System.Environment (getArgs)
import Text.Printf (printf)

type Point = (Double, Double)


closestPairDistance :: [Point] -> Double

format :: Double -> String
format dist
  | dist > 10000 = "INFINITY"
  | otherwise    = printf "%.4f" d

--| split string with char 
split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
              [] -> []
              ys -> w : split p zs
                where (w, zs) = break p ys

parser :: String -> [[Point]]
parser s = [map read]
    where ss = map words $ lines s
          xss = split (\x -> length x == 1) ss



main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = parser contents
    let outputs = map closestPair inputs
    mapM putStrLn $ map format outputs
