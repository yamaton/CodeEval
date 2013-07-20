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
This is closest pair of points problem. See the article for better performance.
http://en.wikipedia.org/wiki/Closest_pair_of_points_problem
-}

import System.Environment (getArgs)
import Data.List (sort)
import Text.Printf (printf)

type Point = (Double, Double)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs  
  | n == 1    = map (:[]) xs 
  | otherwise = helper n (length xs) xs    
    where
      helper k l ys@(z:zs)        
        | k < l     = map (z :) (combinations (k-1) zs)
                         ++ combinations k zs
        | k == l    = [ys]
        | otherwise = []


closestPairDistance :: [Point] -> Double
closestPairDistance pts
  | length pts < 4 = sqrt $ minimum [distSquared p1 p2 | [p1, p2] <- combinations 2 pts]  
  | otherwise      = if (not . null) dsLR
                       then min (minimum dsLR) dMin
                       else dMin
    where 
      distSquared :: Point -> Point -> Double
      distSquared (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2
      points = sort pts
      threshold = (length pts) `div` 2
      (pointsL, pointsR) = splitAt threshold points
      (xL0, _) = last pointsL
      (xR0, _) = head pointsR
      xMid = (xL0 + xR0) / 2

      dMin = min (closestPairDistance pointsL) (closestPairDistance pointsR)
      selectedPointsL = [(xL, yL) | (xL, yL) <- pointsL, (xMid - xL) < dMin] 
      selectedPointsR = [(xR, yR) | (xR, yR) <- pointsR, (xR - xMid) < dMin]
      dsLR = [distSquared (xL, yL) (xR, yR)| 
                (xL, yL) <- selectedPointsL, 
                (xR, yR) <- selectedPointsR,
                abs (yL - yR) < dMin]


format :: Double -> String
format dist
  | dist > 10000 = "INFINITY"
  | otherwise    = printf "%.4f" dist


-- | split string with char 
split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
  [] -> []
  ys -> w : split p zs
    where (w, zs) = break p ys


reader :: String -> [[Point]]
reader s = [[(read a, read b)| [a, b] <- xs] | xs <- xss]
  where 
    ss = map words $ lines s
    xss = split (\x -> length x == 1) ss


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = reader contents
    let outputs = map closestPairDistance inputs
    --mapM print inputs
    mapM_ (putStrLn . format) outputs
