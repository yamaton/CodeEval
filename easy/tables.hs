{-
tables.hs

Created by Yamato Matsuoka on 2013-07-03.

Description
------------
Print out the grade school multiplication table upto 12*12.

Input sample
-------------
None

Output sample
--------------
Print out the table in a matrix like fashion, each number formatted to a width of 4 (The numbers are right-aligned and strip out leadeing/trailing spaces on each line). The first 3 line will look like:  e.g.
```
1   2   3   4   5   6   7   8   9  10  11  12
2   4   6   8  10  12  14  16  18  20  22  24
3   6   9  12  15  18  21  24  27  30  33  36
```
-}
import Text.Printf (printf)

main = do 
    let table = [[i * j | i <- [1 .. 12]] | j <- [1 .. 12]] :: [[Int]]
    let output = [concat [printf "%4d" n | n <- line] | line <- table]
    mapM (putStrLn . (drop 2)) output
