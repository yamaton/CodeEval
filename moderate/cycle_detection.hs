{-
cycle_detection.hs

Created by Yamato Matsuoka on 2013-07-04.

Description
------------
Given a sequence, write a program to detect cycles within it.

Input sample
-------------
A file containing a sequence of numbers (space delimited). The file can have multiple such lines. e.g
```
2 0 6 3 1 6 3 1 6 3 1
```
Ensure to account for numbers that have more than one digit eg. 12. If there is no sequence, ignore that line.

Output sample
--------------
Print to stdout the first sequence you find in each line. Ensure that there are no trailing empty spaces on each line you print. e.g.
```
6 3 1
```

-}

import System.Environment (getArgs)
import Data.List (tails, find, elemIndices)
import Data.Maybe (fromJust, isJust)
import Control.Applicative ((<$>), (<*>)) 

detectCycle :: Eq a => [a] -> Maybe [a]
detectCycle xs = take <$> (periodicityLength <$> zs) <*> zs
  where zs = find (\ys -> periodicityLength ys > 0) (tails xs)

-- periodicityLength xs = 0 if xs is aperiodic
periodicityLength :: Eq a => [a] -> Int
periodicityLength []        = 0
periodicityLength [x]       = 0
periodicityLength ys@(x:xs) = if (null out || 2 * (head out) > len) then 0 else (head out)
  where
    len = length ys
    _:idx = elemIndices x ys
    isPeriodicEvery p = all (\i -> ys !! i == ys !! (i+p)) [0 .. len - p - 1]
    out = dropWhile (not . isPeriodicEvery) idx

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = [map read (words line) | line <- filter (not . null) (lines contents)] :: [[Int]]
    let outputs = filter isJust $ map detectCycle inputs :: [Maybe [Int]]
    mapM_ (putStrLn . unwords . map show . fromJust) outputs
