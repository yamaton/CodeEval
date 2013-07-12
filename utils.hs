-- Haskell toolbox for CodeEval Problems

import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Data.List (intercalate)
import Control.Monad (forM, replicateM)
import Control.Monad.State (evalState, get, put)
import Data.Map (fromListWith, toList)


-- split string with char 
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
              "" -> []
              s' -> w : split c s''
                where (w, s'') = break (== c) s'

-- join elements in [String] by putting String in between them
join :: String -> [String] -> String
join = intercalate


-- combinations
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


-- Equivalent to `combinations_with_relacement` in itertools of Python.
combinationsWithReplacement :: (Eq a) => Int -> [a] -> [[a]]


-- frequency (occurrence) of element
-- http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell
tally :: (Ord a) => [a] -> [(a, Int)]
tally xs = toList $ fromListWith (+) [(x, 1)| x <- xs]


-- Cartesian product
-- Tuples in Mathematica
cartProd :: Int -> [a] -> [[a]]
cartProd = replicateM


--| Fibonacci number
fibonacci :: Int -> Int
fibonacci n = flip evalState (0,1) $ do
  forM [0 .. (n-1)] $ \_ -> do
    (a, b) <- get
    put (b, a + b)
  (a, b) <- get
  return a


--| integer to digits
integerDigits :: Int -> [Int]
integerDigits n = reverse . map (`mod` 10) $ takeWhile (> 0) $ iterate (`div` 10) n

integerDigits' :: Int -> [Int]
integerDigits' n = map (read . (:[])) (show n)


--| digits to integer
fromDigits :: [Int] -> Int
fromDigits xs = foldl1 (\i j -> 10 * i + j) xs

fromDigits' :: [Int] -> Int
fromDigits' xs = read $ concatMap show xs


factorInteger :: Int -> [Int]


divisors :: Int -> [Int]

--| check if integer is palindrome
isPalindrome :: Int -> Bool
isPalindrome n = let s = show n
                 in (s == reverse s)


--| check if integer is prime number
isPrime :: Int -> Bool
isPrime 2 = True
isPrime n
  | n < 2     = False
  | even n    = False
  | otherwise = all (\p -> mod n p /= 0) [3, 5 .. ub]
                where ub = (floor . sqrt . fromIntegral) n
                

-- integer to binary string  
intToBin :: Int -> String
intToBin n = showIntAtBase 2 intToDigit n ""


-- hex string to integer
hexToInt :: String -> Int
hexToInt s = (fst . head) readHex s


-- conunt element in list
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

