{-
double_squares.hs

Created by Yamato Matsuoka on 2012-07-04.

Description
------------
Credits: This challenge appeared in the Facebook Hacker Cup 2011.

A double-square number is an integer X which can be expressed as the sum of two perfect squares. For example, 10 is a double-square because 10 = 3^2 + 1^2. Your task in this problem is, given X, determine the number of ways in which it can be written as the sum of two squares. For example, 10 can only be written as 3^2 + 1^2 (we don't count 1^2 + 3^2 as being different). On the other hand, 25 can be written as 5^2 + 0^2 or as 4^2 + 3^2.

NOTE: Do NOT attempt a brute force approach. It will not work. The following constraints hold: 

0 <= X <= 2147483647
1 <= N <= 100

Input sample
-------------
You should first read an integer N, the number of test cases. The next N lines will contain N values of X.
```
5
10
25
3
0
1
```

Output sample
--------------
e.g.
```
1
2
0
1
1
```
-}

import System.Environment (getArgs)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM_)
import Data.Array.ST (newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, assocs)
import Data.Map (fromListWith, toList)
import Data.List (sort)

---- Eratosthenes sieve
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  let maxP = floor . sqrt $ fromIntegral n
  sieveTF <- newArray (2, n) True 
  forM_ [2..maxP] $ \p -> do
    isPrime <- readArray sieveTF p
    when isPrime $ do
      forM_ [p*p, p*p+p .. n] $ \q -> do
        writeArray sieveTF q False
  return sieveTF

-- | 
-- >>> primesTo 20
-- [2,3,5,7,11,13,17,19]
primesTo :: Int -> [Int]
primesTo n
  | n < 2     = []
  | otherwise = [i | (i,True) <- assocs $ sieve n]

-- |
-- >>> factorInteger 5
-- [(5,1)]
factorInteger :: Int -> [(Int, Int)]
factorInteger 0 = [(0, 1)]
factorInteger 1 = [(1, 1)]
factorInteger n = tally $ factor n
  where
    primes = primesTo $ (round . sqrt) (fromIntegral n)
    factor 1 = []
    factor p = k : factor (p `div` k)
      where 
        divisors = dropWhile (\q -> p `mod` q /= 0) primes
        k = if null divisors then p else head divisors

-- | 
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]
-- >>> divisors 151
-- [1,151]
divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = sort [product xs | xs <- cartesianProduct factors]
  where factors = [ map (n^) [0..pow] | (n, pow) <- factorInteger n ]


-- |
-- >>> tally "aaaddbcdabbbaf"
-- [('a',5),('b',4),('c',1),('d',3),('f',1)]
tally :: Ord a => [a] -> [(a, Int)]
tally xs = toList $ fromListWith (+) [(x, 1)| x <- xs]


-- |
-- >>> cartesianProduct [[1,2,3], [7,8], [9]]
-- [[1,7,9],[1,8,9],[2,7,9],[2,8,9],[3,7,9],[3,8,9]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = foldr (\xs acc -> (:) <$> xs <*> acc) [[]]


-- |
-- >>> isSquare 9
-- True
-- >>> isSquare 10
-- False 
isSquare :: Int -> Bool
isSquare n = n == ((round . sqrt . fromIntegral) n)^2


-- Use Jacobi's two square theorem
countSquareSums :: Int -> Int
countSquareSums 0 = 1
countSquareSums n = (total + flg) `div` 2
  where
    ds = divisors n
    total = sum [1 | i <- ds, i `mod` 4 == 1] - sum [1 | i <- ds, i `mod` 4 == 3]
    flg = (if isSquare n then 1 else 0) + (if even n && isSquare (n `div` 2) then 1 else 0)


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs  = map read $ filter (not . null) $ tail $ lines contents
  let outputs = map countSquareSums inputs
  mapM_ print outputs
