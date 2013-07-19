{-
Word Search
===========

Description
-----------
Given a 2D board and a word, find if the word exists in the grid. The word can be constructed from letters of sequentially adjacent cell, where adjacent cells are those horizontally or vertically neighboring. The same letter cell may not be used more than once.

Input sample
-------------
The board to be used may be hard coded as:

[
[ABCE],
[SFCS],
[ADEE]
]

Your program should accept as its first argument a path to a filename. Each line in this file contains a word. e.g.
```
ASADB
ABCCED
```

Output sample
--------------
Print out True if the word exists in the board, False otherwise. e.g.
```
False
True
```

-}
import System.Environment (getArgs)

searchWord :: [String] -> String -> Bool
searchWord = undefined


main = do
  f:_ <- getArgs
  contents <- readFile f
  let inputs = lines contents
  let grid = ["ABCE","SFCS","ADEE"]
  let outputs = map (searchWord grid) inputs
  mapM_ print outputs