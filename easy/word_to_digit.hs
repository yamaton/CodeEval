{-
Word to Digit
=============
Challenge Description
----------------------
Having a string representation of a set of numbers you need to print this numbers.

All numbers are separated by semicolon. There are up to 20 numbers in one line. The numbers are "zero" to "nine"

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. E.g.
```
zero;two;five;seven;eight;four
three;seven;eight;nine;two
```

Output sample
--------------
Print numbers in the following way:
```
025784
37892
```
-}

import System.Environment (getArgs)

-- | Split string with specified char 
-- >>> split ',' "aa,bc,cd,e"
-- ["aa","bc","cd","e"]
split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

wordToDigit :: [String] -> String
wordToDigit = map helper
  where
    helper :: String -> Char
    helper "zero"  = '0'
    helper "one"   = '1'
    helper "two"   = '2'
    helper "three" = '3'
    helper "four"  = '4'
    helper "five"  = '5'
    helper "six"   = '6'
    helper "seven" = '7'
    helper "eight" = '8'
    helper "nine"  = '9'
    helper _       = error "bad word!"

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map (split ';') $ lines contents
  let outputs = map wordToDigit inputs
  mapM_ putStrLn outputs