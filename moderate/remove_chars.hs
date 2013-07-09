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

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
    "" -> []
    s' -> w : s'' where (w, s'') = break (== c) s'


def remove_chars(entry):
    text, chars = entry
    return "".join(c for c in text if c not in chars)

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[s.strip() for s in line.rstrip().split(",")] for line in f]
    
    out = (remove_chars(entry) for entry in data)
    print "\n".join(out)


