{-
String Searching
=================
Created by Yamato Matsuoka on 2013-07-18.

Description
-----------
You are given two strings. Determine if the second string is a substring of the first (Do NOT use any substr type library function). The second string may contain an asterisk(*) which should be treated as a regular expression i.e. matches zero or more characters. The asterisk can be escaped by a \ char in which case it should be interpreted as a regular '*' character. To summarize: the strings can contain alphabets, numbers, * and \ characters.

Input sample
------------
File containing two comma delimited strings per line. e.g. 
```
Hello,ell
This is good, is 
CodeEval,C*Eval
Old,Young
```

Output sample
--------------
If the second string is indeed a substring of the first, print out a 'true'(lowercase), else print out a 'false'(lowercase), one per line. e.g.
```
true
true
true
false
```

[Comment]
  Haskell in CodeEval does not have Text.Regex.Posix module.
-}

import System.Environment (getArgs)
import Text.Regex.Posix ((=~))

toRegex :: String -> String
toRegex s
  | '*' `notElem` s  = s
  | otherwise        = modified ++ toRegex latter
    where
      (former, _:latter) = span (/= '*') s
      idx    = length former
      modified = former ++ if last former == '\\' then "*" else ".*"

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

boolToStr :: Bool -> String
boolToStr True  = "true"
boolToStr False = "false"

isContained :: String -> String -> Bool
isContained s t = s =~ toRegex t
    
main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map (split ',') $ lines contents
  let outputs = [isContained s t | [s, t] <- inputs]
  mapM_ (putStrLn . boolToStr) outputs
