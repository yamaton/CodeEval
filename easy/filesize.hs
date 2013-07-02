{-
filesize.hs

Created by Yamato Matsuoka on 2013-07-02

# Description
Print the size of a file in bytes.

## Input sample
Path to a filename. e.g. 
```
foo.txt
```

## Output sample:
Print the size of the file in bytes. e.g.
```
55
```
-}


import System.Environment (getArgs)
import System.Posix.Files (getFileStatus, fileSize)

main = do 
    args <- getArgs
    let filename = head args
    fs <- getFileStatus filename
    print (fileSize fs)



