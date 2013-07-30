{-
remove_chars.hs

Created by Yamato Matsuoka on 2013-07-08

Description
------------
Write a program to remove specific characters from a string.

Input sample
-------------
The first argument will be a text file containing an input string followed by a comma and then the characters that need to be scrubbed. e.g. 
```
how are you, abc
hello world, def
```

Output sample
--------------
Print to stdout, the scrubbed strings, one per line. Trim out any leading/trailing whitespaces if they occur. e.g.
```
how re you
hllo worl
```

-}

import System.Environment (getArgs)

reader :: String -> (String, String)
reader s = (former, drop 2 latter)
    where (former, latter) = break (== ',') s

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = map reader $ lines contents
    let outputs = [filter (`notElem` s) text | (text, s) <- inputs]
    mapM_ putStrLn outputs
