{-
str_substitution.hs

Created by Yamato Matsuoka on 2013-07-11.

Description
------------
Credits: This challenge was contributed by Sam McCoy

Given a string S, and a list of strings of positive length, F1,R1,F2,R2,...,FN,RN, proceed to find in order the occurrences (left-to-right) of Fi in S and replace them with Ri. All strings are over alphabet { 0, 1 }. Searching should consider only contiguous pieces of S that have not been subject to replacements on prior iterations. An iteration of the algorithm should not write over any previous replacement by the algorithm.

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain a string, then a semicolon and then a list of comma separated strings.eg.
```
10011011001;0110,1001,1001,0,10,11
```

Output sample
-------------
For each line of input, print out the string after substitutions have been made.eg.
```
11100110
```

For the curious, here are the transitions for the above example: 10011011001 => 10100111001 [replacing 0110 with 1001] => 10100110 [replacing 1001 with 0] => 11100110 [replacing 10 with 11] => 11100110

-}

import System.Environment (getArgs)
import Data.Text (Text, pack, unpack, splitOn, intercalate)

makePairs :: [a] -> [(a,a)]
makePairs xs = 
  case splitAt 2 xs of
    ([], _)      -> []
    ([a,b], zs)  -> (a, b) : makePairs zs


---- This replacement has recursive structure
strSubstitution :: Text -> [(Text, Text)] -> Text
strSubstitution target [] = target
strSubstitution target ((from,to):xs) = intercalate to $ map (flip strSubstitution xs) (splitOn from target)


reader :: String -> (Text, [(Text, Text)])
reader s = (former, makePairs (splitOn (pack ",") latter))
  where [former, latter] = splitOn (pack ";") (pack s)

main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map reader $ lines contents
  let outputs = [strSubstitution target rules | (target, rules) <- inputs]
  mapM_ (putStrLn . unpack) outputs
