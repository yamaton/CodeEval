{-
levenshtein_distance.hs

Created by Yamato Matsuoka on 2013-07-12.

Two words are friends if they have a Levenshtein distance of 1 (For details see http://en.wikipedia.org/wiki/Levenshtein_distance). That is, you can add, remove, or substitute exactly one letter in word X to create word Y. A word’s social network consists of all of its friends, plus all of their friends, and all of their friends’ friends, and so on. Write a program to tell us how big the social network for the word 'hello' is, using this word list https://raw.github.com/codeeval/Levenshtein-Distance-Challenge/master/input_levenshtein_distance.txt

Input sample
-------------
Your program should accept as its first argument a path to a filename. The input file contains the word list. This list is also available at https://raw.github.com/codeeval/Levenshtein-Distance-Challenge/master/input_levenshtein_distance.txt.

Output sample
---------------
Print out how big the social network for the word 'hello' is. e.g. The social network for the word 'abcde' is 4846.

[Comment]
"hello" has 4844 friends (30 sec on GHC, 14 sec on GHC w/ -O2 option)
because of the simple-minded O(N^2) algorithm.

I think I need to structure word list at first, maybe to N-gram or something similar.

 
-}

import System.Environment (getArgs)
import Data.List ((\\))

isFriend :: String -> String -> Bool
isFriend s t
  | lenS == lenT     = 1 == length (filter (\(x,y) -> x /= y) $ zip s t)
  | lenS == lenT + 1 = drop (n + 1) s == drop n t
  | lenS == lenT - 1 = isFriend t s
  | otherwise        = False
    where
      lenS = length s
      lenT = length t
      n = length $ takeWhile (\(x, y) -> x == y) (zip s t)

-- (-1) because the initial word is not counted
countFriendNetwork :: String -> [String] -> Int
countFriendNetwork s xs = length (friendNetwork [[s]] xs) - 1

friendNetwork :: [[String]] -> [String] -> [String]
friendNetwork ([]:past) pool = concat past
friendNetwork xs        []   = concat xs
friendNetwork xss@(xs:_) pool = friendNetwork (next:xss) (pool \\ next)
  where next = filter (\p -> any (isFriend p) xs) pool


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let wordList = lines contents
  print $ countFriendNetwork "hello" wordList
