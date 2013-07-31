{-
For this challenge you are given an encrypted message and a key. You have to determine the encryption and encoding technique and then print out the corresponding plaintext message. You can assume that the plaintext corresponding to this message, and all messages you must handle, will be comprised of only the characters A-Z and spaces; no digits or punctuation.

Input sample
-------------
There is no input for this program. The encrypted message and key is:
```
message: "012222 1114142503 0313012513 03141418192102 0113 2419182119021713 06131715070119",
keyed_alphabet: "BHISOECRTMGWYVALUZDNFJKPQX"
```

Output sample
---------------
Print out the plaintext message. (in CAPS)
-}

import Data.List (intercalate, elemIndex)
import Data.Char (ord, chr)
import Data.Maybe (fromJust)


shiftCharBy :: Int -> Char -> Char
shiftCharBy n c =  chr $ ord 'A' + mod (ord c - ord 'A' + n) 26


readEvery :: Int -> String -> [Int]
readEvery n s = map read $ splitEveryN s
  where 
    splitEveryN t = case take n t of
      "" -> []
      chunk -> chunk : splitEveryN (drop n t)


decode :: Int -> Char
decode n = ['A' .. 'Z'] !! idx
  where 
    key = "BHISOECRTMGWYVALUZDNFJKPQX"
    c = chr (ord 'A' + n) 
    idx = fromJust (elemIndex c key)


main = do 
  let message = "012222 1114142503 0313012513 03141418192102 0113 2419182119021713 06131715070119"
  let input = map (readEvery 2) (words message)
  let output = unwords [map decode chunk | chunk <- input]
  putStrLn output
    