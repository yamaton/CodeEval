{-
reverse_words.hs

Created by Yamato Matsuoka on 2013-07-03.

Description:
------------
Write a program to reverse the words of an input sentence.

Input sample:
-------------
The first argument will be a text file containing multiple sentences, one per line. Possibly empty lines too. e.g. 
```
Hello World
Hello CodeEval
```

Output sample:
--------------
Print to stdout, each line with its words reversed, one per line. Empty lines in the input should be ignored. Ensure that there are no trailing empty spaces on each line you print. e.g.
```
World Hello
CodeEval Hello
```
-}

import System.Environment (getArgs)

main = do 
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = lines contents
    let outputs = map (reverse.words) inputs
    mapM putStrLn (map unwords outputs)



