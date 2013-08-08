{-
grid_walk.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
-------------
There is a monkey which can walk around on a planar grid. The monkey can move one space at a time left, right, up or down. That is, from (x, y) the monkey can go to (x+1, y), (x-1, y), (x, y+1), and (x, y-1). Points where the sum of the digits of the absolute value of the x coordinate plus the sum of the digits of the absolute value of the y coordinate are lesser than or equal to 19 are accessible to the monkey. For example, the point (59, 79) is inaccessible because 5 + 9 + 7 + 9 = 30, which is greater than 19. Another example: the point (-5, -7) is accessible because abs(-5) + abs(-7) = 5 + 7 = 12, which is less than 19. How many points can the monkey access if it starts at (0, 0), including (0, 0) itself?

Input sample
-------------
There is no input for this program.

Output sample
--------------
Print the number of points the monkey can access. It should be printed as an integer — for example, if the number of points is 10, print "10", not "10.0" or "10.00", etc.



[Strategy]
Restrict ourselves in 1/8 of the x-y plane only (0<=y<=x) due to the symmetry.

-}
type Coord = (Int, Int)

-- |
-- >>> digitSum 5979
-- 30
digitSum :: Int -> Int
digitSum = sum . map (read . (:[])) . show

isAccessible :: Coord -> Bool
isAccessible (x, y) = isInDomain && isDigitSumOK
  where
    isInDomain = y <= x
    isDigitSumOK = (digitSum x + digitSum y) <= 19


scanGrid :: [Coord]
scanGrid = scanHelper [(0, 0)] [(0, 0)]
  where 
    scanHelper :: [Coord] -> [Coord] -> [Coord]
    scanHelper []            visited = visited
    scanHelper ((x,y):stack) visited 
      | null nextAllowed = scanHelper stack visited
      | otherwise        = scanHelper (nextAllowed ++ stack) (nextAllowed ++ visited)
        where nextAllowed = filter (\p -> p `notElem` visited && isAccessible p) [(x+1, y), (x, y+1)]


---- - 1 / + 1 : excluding the coordinate (0,0) first and then add it later
adjustCount :: [Coord] -> Int
adjustCount points =  8 * innerCount + 4 * edgeCount + 1 
  where
    edgeCount = sum [1 | (x,y) <- points, x == y || y == 0] - 1
    innerCount = length points - edgeCount - 1


countGridWalk :: Int
countGridWalk = adjustCount scanGrid

main = do
  print countGridWalk
