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
type Field = [Point]


robotMovements :: Field -> Point -> Point -> Int
robotMovements field start goal = helper [[start]] 0  
  where
    helper :: [Path] -> Int -> Int
    helper  []   goalCount = goalCount
    helper stack goalCount = helper nextStack updateCount  
      where
        nextStackTmp = concatMap (moveRobot field) stack
        nextStack    = filter (\(curr:past) -> curr /= goal) nextStackTmp
        updateCount = goalCount + (length nextStackTmp - length nextStack)


moveRobot :: Field -> Path -> [Path]
moveRobot field path = map (:path) nextPoints 
  where
    ((i, j):past) = path
    isValid p = (p `elem` field) && (p `notElem` past)
    nextPoints = filter isValid [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]


main = do
  let field = (\a b -> (a, b)) <$> [0..3] <*> [0..3]
  let start = (0,0)
  let goal  = (3,3)
  print $ robotMovements field start goal

