{-
loweset_unique.hs

Created by Yamato Matsuoka on 2013-07-12

Lowest Unique Number
====================
Challenge Description
----------------------
There is a game where each player picks a number from 1 to 9, writes it on a paper and gives to a guide. A player wins if his number is the lowest unique. We may have 10-20 players in our game.

Input sample
-------------
Your program should accept as its first argument a path to a filename.

You're a guide and you're given a set of numbers from players for the round of game. E.g. 2 rounds of the game look this way:
```
3 3 9 1 6 5 8 1 5 3
9 2 9 9 1 8 8 8 2 1 1
```

Output sample
--------------
Print a winner's position or 0 in case there is no winner. In the first line of input sample the lowest unique number is 6. So player 5 wins.
```
5
0
```
-}

import System.Environment (getArgs)
import Data.List (sort, elemIndex)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromJust)

tally :: (Ord a) => [a] -> [(a, Int)]
tally xs = sort $ toList $ fromListWith (+) [(x, 1)| x <- xs]

lowestUniqueIdx :: [Int] -> Int
lowestUniqueIdx xs = 
    if null singleAppearences
        then 0 
        else let n = (fst . head) singleAppearences
             in (fromJust $ elemIndex n xs) + 1  -- count from 1
    where singleAppearences = filter (\x -> snd x == 1) $ tally xs


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = [map read (words line) | line <- lines contents]
    let outputs = map lowestUniqueIdx inputs
    mapM print outputs
