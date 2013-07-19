{-
email_regex.hs

Created by Yamato Matsuoka on 2013-07-04.

Description
-----------
You are given several strings that may/may not be valid emails. You should write a regular expression that determines if the email id is a valid email id or not. You may assume all characters are from the english language.

Input sample
-------------
Your program should accept as its first argument a filename. This file will contain several text strings, one per line. Ignore all empty lines. eg.
```
foo@bar.com
this is not an email id
admin#codeeval.com
good123@bad.com
```

Output sample
--------------
Print out 'true' (all lowercase) if the string is a valid email. Else print out 'false' (all lowercase) .e.g.
```
true
false
false
true
```

[NOTE] Text.Regex.Posix is installed with haskell-platform and not included in vanilla ghc.
       This can also be installed with cabal.
       >> cabal install regex-posix

    CodeEval, as of 2013-07-04, does not have the regex module in their system.
-}

import System.Environment (getArgs)
import Text.Regex.Posix ((=~))

isEmailAddress :: String -> Bool
isEmailAddress s = (s =~ "[A-Za-z0-9._+-]+@[A-Za-z0-9-]+(\\.[a-zA-Z0-9]+){1,2}")

boolToString :: Bool -> String
boolToString True  = "true"
boolToString False = "false"

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = lines contents
    let outputs = map (boolToString . isEmailAddress) inputs
    mapM_ putStrLn outputs

