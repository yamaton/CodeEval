{-
Valid parentheses 
==================
Description
------------
Given a string comprising just of the characters (,),{,},[,] determine if it is well-formed or not.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a string comprising of the characters mentioned above. e.g.
```
()
([)]
```

Output sample
--------------
Print out True or False if the string is well-formed e.g.
```
True
False
```
-}

import System.Environment (getArgs)
import Control.Monad (foldM)


isValidParenthesis :: String -> Bool
isValidParenthesis s = foldM helper "" s == Just ""
  where
    helper :: String -> Char -> Maybe String
    helper   ""      c   = if c `elem` "({[" then Just [c] else Nothing
    helper (s:stack) ')' = if s == '(' then Just stack else Nothing
    helper (s:stack) '}' = if s == '{' then Just stack else Nothing    
    helper (s:stack) ']' = if s == '[' then Just stack else Nothing    
    helper   stack   c   = Just (c:stack)


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = lines contents
    let outputs = map isValidParenthesis inputs
    mapM_ print outputs

