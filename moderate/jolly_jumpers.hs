{-
jolly_jumpers.hs

Created by Yamato Matsuoka on 2013-07-05.

Description
------------
Credits: Programming Challenges by Steven S. Skiena and Miguel A. Revilla

A sequence of n > 0 integers is called a jolly jumper if the absolute values of the differences between successive elements take on all possible values 1 through n - 1. eg.
```
1 4 2 3
```
is a jolly jumper, because the absolute differences are 3, 2, and 1, respectively. The definition implies that any sequence of a single integer is a jolly jumper. Write a program to determine whether each of a number of sequences is a jolly jumper.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 3000 followed by n integers representing the sequence. The integers are space delimited.

Output sample
--------------
For each line of input generate a line of output saying 'Jolly' or 'Not jolly'.
-}

import System.Environment (getArgs)
import Data.List (sort)

isJolly :: [Int] -> Bool
isJolly xs
    | n == 1    = True
    | otherwise = [1 .. (n-1)] == (sort . map abs $ zipWith (-) (tail xs) (init xs))
        where n = length xs

boolToJolly :: Bool -> String
boolToJolly True = "Jolly"
boolToJolly False = "Not jolly"

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = [map read $ tail (words line) | line <- lines contents]
    let outputs = map (boolToJolly . isJolly) inputs
    mapM putStrLn outputs

