{-
cash_register.hs

Created by Yamato Matsuoka on 2012-07-03.

Description
------------
The goal of this challenge is to design a cash register program. You will be given two float numbers. The first is the purchase price (PP) of the item. The second is the cash (CH) given by the customer. Your register currently has the following bills/coins within it:

'PENNY': .01,
'NICKEL': .05,
'DIME': .10,
'QUARTER': .25,
'HALF DOLLAR': .50,
'ONE': 1.00,
'TWO': 2.00,
'FIVE': 5.00,
'TEN': 10.00,
'TWENTY': 20.00,
'FIFTY': 50.00,
'ONE HUNDRED': 100.00

The aim of the program is to calculate the change that has to be returned to the customer.

Input sample
-------------
Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains two numbers which are separated by a semicolon. The first is the Purchase price (PP) and the second is the cash(CH) given by the customer. eg.
```
15.94;16.00
17;16
35;35
45;50
```

Output sample
-------------
For each set of input produce a single line of output which is the change to be returned to the customer. In case the CH < PP, print out ERROR. If CH == PP, print out ZERO. For all other cases print the amount that needs to be returned, in terms of the currency values provided. The output should be alphabetically sorted. eg.
```
NICKEL,PENNY
ERROR
ZERO
FIVE
```
-}

import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.Map (fromList, lookupLE)
import Data.List (intercalate, sort)


countBillAndCoins :: Int -> [String]
countBillAndCoins 0    = []
countBillAndCoins cent = name : (countBillAndCoins (cent - amount))
  where
    dict = fromList [(10000, "ONE HUNDRED"), (5000, "FIFTY"), (2000, "TWENTY"), 
         (1000, "TEN"), (500, "FIVE"), (200, "TWO"), (100, "ONE"), (50, "HALF DOLLAR"),
         (25, "QUARTER"), (10, "DIME"), (5, "NICKEL"), (1, "PENNY")]
    (amount, name) = fromJust $ lookupLE cent dict


findChange :: Double -> Double -> String
findChange pp ch
  | pp > ch   = "ERROR"
  | pp == ch  = "ZERO"
  | otherwise = intercalate "," $ sort (countBillAndCoins changeInCent)
    where 
      [ppInt, chInt] = (map (\x -> round (100 * x)) [pp, ch] :: [Int])
      changeInCent = chInt - ppInt


split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs  = [map read (split ';' s) | s <- lines contents]
  let outputs = [findChange pp ch | [pp, ch] <- inputs]
  mapM_ putStrLn outputs

