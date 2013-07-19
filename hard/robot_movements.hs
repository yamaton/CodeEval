{-
Robot Movements
===============

Description
------------
A robot is located at the top-left corner of a 4x4 grid. The robot can move either up, down, left, or right, but can not visit the same spot twice. The robot is trying to reach the bottom-right corner of the grid.

Input sample
-------------
There is no input for this program.

Output sample
--------------
Print out the unique number of ways the robot can reach its destination. (The number should be printed as an integer whole number eg. if the answer is 10 (its not !!), print out 10, not 10.0 or 10.00 etc)

-}

import Control.Applicative ((<$>), (<*>))

type Point = (Int, Int)
type Path = [Point]


robotMovements :: [Point] -> Point -> Point -> Int
robotMovements field start goal = helper [[start]] 0
  where
    helper :: [Path] -> Int -> Int
    helper [[]] count = count
    helper stack count = helper nextStack nextCount
      where
        moves :: Path -> [Path]
        moves path = map (:path) nextPoints
          where 
            ((i, j):past) = path
            isValid p = (p `elem` field) && (p `notElem` past)
            nextPoints = filter isValid [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
        nextStackTmp = concatMap moves stack  -------------- [BUG !?]
        nextStack    = filter (\(curr:past) -> curr /= goal) nextStackTmp
        nextCount = count + (length nextStackTmp - length nextStack)

main = do
  let field = (\a b -> (a, b)) <$> [0..1] <*> [0..1]
  let start = (0,0)
  let goal  = (0,1)
  print $ robotMovements field start goal
