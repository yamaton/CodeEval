{-
longestlines.hs

Created by Yamato Matsuoka on 2013-07-06.

Description
------------
Write a program to read a multiple line text file and write the 'N' longest lines to stdout. Where the file to be read is specified on the command line.

Input sample
-------------
Your program should read an input file (the first argument to your program). The first line contains the value of the number 'N' followed by multiple lines. You may assume that the input file is formatted correctly and the number on the first line i.e. 'N' is a valid positive integer.e.g.
```
2
Hello World

CodeEval
Quick Fox
A
San Francisco
```

Output sample
--------------
The 'N' longest lines, newline delimited. Do NOT print out empty lines. Ignore all empty lines in the input. Ensure that there are no trailing empty spaces on each line you print. Also ensure that the lines are printed out in decreasing order of length i.e. the output should be sorted based on their length e.g.
```
San Francisco
Hello World
```
-}

import System.Environment (getArgs)
import GHC.Exts (sortWith)

parser :: [String] -> (Int, [String])
parser xs = (read (head xs), tail xs)

main = do 
  args <- getArgs
  contents <- readFile (head args)
  let (n, inputs) = parser $ lines contents
  let outputs = take n $ reverse $ sortWith length inputs
  mapM_ putStrLn outputs
