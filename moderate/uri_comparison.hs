{-
Created by Yamato Matsuoka on 2013-07-09

URI Comparison
===============
Description
-------------
Determine if two URIs match. For the purpose of this challenge, you should use a case-sensitive octet-by-octet comparison of the entire URIs, with these exceptions:

1. A port that is empty or not given is equivalent to the default port of 80
2. Comparisons of host names MUST be case-insensitive
3. Comparisons of scheme names MUST be case-insensitive
4. Characters are equivalent to their % HEX HEX encodings. (Other than typical reserved characters in urls like /,?,@,:,&,= etc)

Input sample
--------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two urls delimited by a semicolon. e.g.
```
http://abc.com:80/~smith/home.html;http://ABC.com/%7Esmith/home.html
```

Output sample
--------------
Print out True/False if the URIs match. e.g.
```
True
```
-}

import System.Environment (getArgs)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Network.URI

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (== c) s'

---- [TODO] Replace `fromJust` with Monad operation
isEquivalentURI :: String -> String -> Bool
isEquivalentURI s t = all (\f -> f ss == f tt) [port, host, scheme, users, path, query, fragment]
  where [ss, tt] = map (fromJust . parseURI . unEscapeString) [s, t]
        port = portModify . uriPort . fromJust . uriAuthority
        host = map toLower . uriRegName . fromJust . uriAuthority        
        scheme = map toLower . uriScheme
        users = uriUserInfo . fromJust . uriAuthority
        path = uriPath
        query = uriQuery
        fragment = uriFragment

portModify :: String -> String
portModify "" = ":80"
portModify ":" = ":80"
portModify s  = s

main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map (split ';') $ lines contents
  let outputs = [isEquivalentURI s1 s2 | [s1, s2] <- inputs]
  mapM_ print outputs

