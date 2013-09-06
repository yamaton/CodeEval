{-
repeated_substr.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
-----------
You are to find the longest repeated substring in a given text. Repeated substrings may not overlap. If more than one substring is repeated with the same length, print the first one you find. (starting from the beginning of the text). NOTE: The substrings can't be all spaces

Input sample
------------
Your program should accept as its first argument a path to a filename. The input file contains several lines. Each line is one test case. Each line contains a test string. eg.
```
banana
```

Output sample
--------------
For each set of input produce a single line of output which is the longest repeated substring. If there is none, print out the string NONE. eg.
```
an
```


banana
cowhollow
hello codeeval
my name is humphery amei
am so uniqe
-}

import System.Environment (getArgs)

repeatedSubstring :: String -> String
repeatedSubstring s = undefined



format :: String  -> String
format "" = "NONE"
format s  = s

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = lines contents
  let outputs = map repeatedSubstring inputs
  mapM_ (putStrLn . format) outputs

