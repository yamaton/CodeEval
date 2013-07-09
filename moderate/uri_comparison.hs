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

