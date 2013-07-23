{-
lcs.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
------------
You are given two sequences. Write a program to determine the longest common subsequence between the two strings(each string can have a maximum length of 50 characters). (NOTE: This subsequence need not be contiguous. The input file may contain empty lines, these need to be ignored.)

Input sample
-------------
The first argument will be a file that contains two strings per line, semicolon delimited. You can assume that there is only one unique subsequence per test case. e.g.
```
XMJYAUZ;MZJAWXU
```

Output sample
-------------
The longest common subsequence. Ensure that there are no trailing empty spaces on each line you print. e.g.
```
MJAU
```

[Comment]
    ver 0.9 ... lcs using Data.List (subsequences) is too slow to pass CodeEval.
-}
import System.Environment (getArgs)
import Data.List (subsequences, intersect)
import GHC.Exts (sortWith)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

lcs :: String -> String -> String
lcs s t = if null xs then "" else (last $ sortWith length xs)
  where xs = intersect (subsequences s) (subsequences t)


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map (split ';') $ lines contents
  let outputs = [lcs s t | [s,t] <- inputs]
  mapM_ putStrLn outputs
