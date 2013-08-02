{-
Poker hands
============
Description
-----------
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way: 

High Card: Highest value card. 
One Pair: Two cards of the same value. 
Two Pairs: Two different pairs. 
Three of a Kind: Three cards of the same value. 
Straight: All cards are consecutive values. 
Flush: All cards of the same suit. 
Full House: Three of a kind and a pair. 
Four of a Kind: Four cards of the same value. 
Straight Flush: All cards are consecutive values of same suit. 
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit. 
The cards are valued in the order: 
2, 3, 4, 5, 6, 7, 8, 9, Ten, Jack, Queen, King, Ace. 

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives. But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared; if the highest cards tie then the next highest cards are compared, and so on. 

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains 2 hands (left and right). Cadrs and hands are separated by space. E.g.
```
6D 7H AH 7S QC 6H 2D TD JD AS
JH 5D 7H TC JS JD JC TS 5S 7S
2H 8C AD TH 6H QD KD 9H 6S 6C
JS JH 4H 2C 9H QH KC 9D 4D 3S
TC 7H KH 4H JC 7D 9S 3H QS 7S
```

Output sample
--------------
Print out the name of the winning hand or "none" in case the hands are equal. E.g.
```
left
none
right
left
right
```

[Credit] The original code is from Udacity Course CS212 Unit 1.
-}

import System.Environment (getArgs)
import Data.List (sort, elemIndex, nub)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)

type Hand = [String] -- example: ["6D", "7H", "AH", "7S", "QC"]
type Rank = Int      

poker :: Hand -> Hand -> String
poker hand1 hand2
  | (not . null) extra = "none"
  | out == hand1       = "left"
  | otherwise          = "right"
    where out:extra = allMax handRank [hand1, hand2]

allMax :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
allMax rankFunc hands = filter (\h -> rankFunc h == oneMax) hands
  where oneMax = last . sort $ map rankFunc hands


handRank :: Hand -> (Rank, [Rank])
handRank hand
  | isStraight ranks && isFlush hand  = (8, [maximum ranks])
  | isKind 4 ranks                    = (7, [kind 4 ranks, kind 1 ranks]) 
  | isKind 3 ranks && isKind 2 ranks  = (6, [kind 3 ranks, kind 2 ranks])
  | isFlush hand                      = (5, ranks)
  | isStraight ranks                  = (4, [maximum ranks])
  | isKind 3 ranks                    = (3, kind 3 ranks : ranks)
  | isTwoPair ranks                   = (2, twoPair ranks ++ ranks)
  | isKind 2 ranks                    = (1, kind 2 ranks : ranks)
  | otherwise                         = (0, ranks)
    where ranks = cardRanks hand


---- example: [14, 14, 4, 4, 2]
cardRanks :: Hand -> [Rank]
cardRanks hand
  | rank == [14,5,4,3,2] = [5,4,3,2,1]
  | otherwise            = rank
    where rank = reverse . sort  $ map (fromJust . (`elemIndex` "--23456789TJQKA") . head) hand

isFlush :: Hand -> Bool
isFlush hand = length (nub suits) == 1
  where suits = map last hand

isStraight :: [Rank] -> Bool
isStraight ranks = maximum ranks - minimum ranks == 4 && length (nub ranks) == 5 

kind :: Int -> [Rank] -> Rank
kind n ranks
  | null outcome = 0
  | otherwise    = head outcome
    where outcome = dropWhile (\x -> count x ranks /= n) ranks

twoPair :: [Rank] -> [Rank]
twoPair ranks
  | 0 /= highPair && highPair /= lowPair  = [highPair, lowPair]
  | otherwise                             = []
    where highPair = kind 2 ranks
          lowPair  = kind 2 (reverse ranks)

isKind n ranks    = 0 /= kind n ranks
isTwoPair ranks = not . null $ twoPair ranks


---- utilities
count :: Int -> [Int] -> Int
count x xs = length $ filter (== x) xs

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = [ splitAt 5 (words line) | line <- lines contents]
  let outputs = [poker hand1 hand2 | (hand1, hand2) <- inputs ]
  mapM_ putStrLn outputs
