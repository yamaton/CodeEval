module Utils where

-- Haskell toolbox for CodeEval Problems
import Data.Char (intToDigit)
import Numeric (showIntAtBase, readHex)
import Data.List (intercalate, nub, sort)
import Control.Monad (forM, replicateM)
import Control.Monad.State (evalState, get, put)
import Data.Map (fromListWith, toList)
import Test.QuickCheck

-- |  Round-Robin 
-- >>> roundRobin ["abc", "d", "ef"]
-- "adebfc"
roundRobin :: [[a]] -> [a]
roundRobin [] = []
roundRobin xs = (map head xs) ++ roundRobin (filter (not . null) (map tail xs))


-- | Split string with specified char 
-- >>> split ',' "aa,bc,cd,e"
-- ["aa","bc","cd","e"]
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

-- | Join elements in [String] by inserting String in between them
-- >>> join ";" ["a", "bc", "  ", "dd  f"]
-- "a;bc;  ;dd  f"
join :: String -> [String] -> String
join = intercalate


-- | Combinations 
-- >>> combinations 2 [1 .. 4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
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


-- | Equivalent to `combinations_with_relacement` in itertools of Python,
--
-- >>> combinationsWithReplacement 2 "abc"
-- ["aa","ab","ac","bb","bc","cc"]
combinationsWithReplacement :: Ord a => Int -> [a] -> [[a]]
combinationsWithReplacement n xs = nub $ map sort $ replicateM n xs


-- | Equivalent to the command in Mathematica
--
-- >>> tuples 2 "abc"
-- ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]
tuples :: Int -> [a] -> [[a]]
tuples = replicateM


-- | Frequency (occurrence) of element
-- http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell
--
-- >>> tally "aaaddbcdabbbaf"
-- [('a',5),('b',4),('c',1),('d',3),('f',1)]
tally :: Ord a => [a] -> [(a, Int)]
tally xs = toList $ fromListWith (+) [(x, 1)| x <- xs]


-- | Fibonacci number
-- >>> fibonacci 10
-- 55
-- >>> fibonacci 20
-- 6765
fibonacci :: Int -> Int
fibonacci n = flip evalState (0,1) $ do
  forM [0 .. (n-1)] $ \_ -> do
    (a, b) <- get
    put (b, a + b)
  (a, b) <- get
  return a


-- | Integer to digits
-- >>> integerDigits 41531
-- [4,1,5,3,1]
integerDigits :: Int -> [Int]
integerDigits n = map (read . (:[])) (show n)
--integerDigits' n = reverse . map (`mod` 10) $ takeWhile (> 0) $ iterate (`div` 10) n

-- | Digits to integer
--
-- >>> fromDigits [1,6,1,5,2]
-- 16152
fromDigits :: [Int] -> Int
fromDigits xs = read $ concatMap show xs

---- For quickCheck...
--prop_integer :: Int -> Bool
--prop_integer n       = 
--  not (n < 0) ==> 
--    n == fromDigits $ integerDigits n

--prop_digits :: [Int] -> Bool
--prop_digits xs  =  
--    xs == integerDigits $ fromDigits xs

factorInteger :: Int -> [Int]
factorInteger = undefined

divisors :: Int -> [Int]
divisors = undefined

-- | Check if integer is palindrome
-- >>> isPalindrome 3
-- True
-- >>> isPalindrome 1051501
-- True
-- >>> isPalindrome 100011
-- False
isPalindrome :: Int -> Bool
isPalindrome n = let s = show n
                 in (s == reverse s)


-- | Check if integer is prime number
-- >>> isPrime 15161
-- True
-- >>> isPrime 15163
-- False
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
  | n < 2     = False
  | even n    = False
  | otherwise = all (\p -> mod n p /= 0) [3, 5 .. ub]
                where ub = (floor . sqrt . fromIntegral) n
                
-- | Integer to binary string  
-- >>> intToBin 100
-- "1100100"
intToBin :: Int -> String
intToBin n = showIntAtBase 2 intToDigit n ""


-- | Hex string to integer
-- >>> hexToInt "ffffff"
-- 16777215
-- 
-- >>> hexToInt "Ab"
-- 171
hexToInt :: String -> Int
hexToInt s = (fst . head) $ readHex s


-- | Conunt elements in list
-- >>> count 'a' "afdadaaaa"
-- 6
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Take Every n elements from a list
-- >>> takeEvery 10 [1..55]
-- [10,20,30,40,50]
takeEvery :: Int -> [a] -> [a]
takeEvery n xs = 
  case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys 


