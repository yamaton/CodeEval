{-
text_dollar.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
------------
Credits: This challenge has been authored by Terence Rudkin

You are given a positive integer number. This represents the sales made that day in your department store. The payables department however, needs this printed out in english. NOTE: The correct spelling of 40 is Forty. (NOT Fourty)

Input sample
-------------
Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains a positive integer. eg.
```
3
10
21
466
1234
```

Output sample
--------------
For each set of input produce a single line of output which is the english textual representation of that integer. The output should be unspaced and in Camelcase. Always assume plural quantities. You can also assume that the numbers are < 1000000000 (1 billion). In case of ambiguities eg. 2200 could be TwoThousandTwoHundredDollars or TwentyTwoHundredDollars, always choose the representation with the larger base i.e. TwoThousandTwoHundredDollars. For the examples shown above, the answer would be:
```
ThreeDollars
TenDollars
TwentyOneDollars
FourHundredSixtySixDollars
OneThousandTwoHundredThirtyFourDollars
```

-}

import System.Environment (getArgs)
import qualified Data.Map as Map

textDollar :: Int -> String
textDollar 1 = "OneDollar"
textDollar n 
    | millions > 0  = (textify millions) ++ "Million" ++ (textDollar (n `mod` 1000000))
    | thousands > 0 = (textify thousands) ++ "Thousand" ++ (textDollar (n `mod` 1000))
    | otherwise     = textify n ++ "Dollars"
      where millions = n `div` 1000000
            thousands = n `div` 1000
            textify :: Int -> String
            textify x 
                | hundreds > 0 = (textOnes Map.! hundreds) ++ "Hundred" ++ (textify (x `mod` 100))
                | tens > 0     = (textTens Map.! (10 * tens)) ++ (textify (x `mod` 10))
                | ones > 0     = textOnes Map.! ones
                | otherwise    = ""
                  where hundreds  = x `div` 100
                        (tens, ones) = if x < 20 
                                           then (0, x) 
                                           else (x `div` 10, x `mod` 10)
                        textTens = Map.fromList [(20,"Twenty"), (30,"Thirty"), (40,"Forty"), (50,"Fifty"), 
                                    (60,"Sixty"), (70,"Seventy"), (80,"Eighty"), (90,"Ninety")]
                        textOnes = Map.fromList [(10,"Ten"), (11,"Eleven"), (12,"Twelve"), (13,"Thirteen"),
                                    (14,"Fourteen"), (15,"Fifteen"), (16,"Sixteen"), (17,"Seventeen"),
                                    (18,"Eighteen"), (19,"Nineteen"),
                                    (1,"One"), (2,"Two"), (3,"Three"), (4,"Four"), (5,"Five"),
                                    (6,"Six"), (7,"Seven"), (8,"Eight"), (9,"Nine")]


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map read $ filter (not . null) $ lines contents
    let outputs = map textDollar inputs
    mapM putStrLn outputs
