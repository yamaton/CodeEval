{-
Roman Numerals 
===============

Challenge Description
---------------------
Many persons are familiar with the Roman numerals for relatively small numbers. The symbols I (capital i), V, X, L, C, D, and M represent the decimal values 1, 5, 10, 50, 100, 500 and 1000 respectively. To represent other values, these symbols, and multiples where necessary, are concatenated, with the smaller-valued symbols written further to the right. For example, the number 3 is represented as III, and the value 73 is represented as LXXIII. The exceptions to this rule occur for numbers having units values of 4 or 9, and for tens values of 40 or 90. For these cases, the Roman numeral representations are IV (4), IX (9), XL (40), and XC (90). So the Roman numeral representations for 24, 39, 44, 49, and 94 are XXIV, XXXIX, XLIV, XLIX, and XCIV, respectively. 

Write a program to convert a cardinal number to a Roman numeral.

Input sample
------------
Your program should accept as its first argument a path to a filename. Input example is the following
```
159
296
3992
24
39
44
49
94
```
Input numbers are in range [1, 3999]

Output sample
-------------
Print out Roman numerals.
```
CLIX
CCXCVI
MMMCMXCII
```

-}

import System.Environment (getArgs)

romanDict :: [(Int, String)]
romanDict = [(1000,"M"), (900, "CM"), (500,"D"), (400,"CD"), (100,"C"), (90, "XC"), (50,"L"), 
             (40, "XL"), (10,"X"), (9, "IX"), (5,"V"), (4, "IV"), (1,"I")]

toRoman :: Int -> String
toRoman 0 = ""
toRoman n = romanLetter ++ toRoman (n - x)
  where (x, romanLetter):_ = dropWhile ((> n) . fst) romanDict

main = do
  f:_ <- getArgs
  contents <- readFile f
  let input = map read $ lines contents
  let output = map toRoman input
  mapM_ putStrLn output

