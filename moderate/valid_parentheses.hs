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

PAIRS = {')': '(', '}': '{', ']': '['}


def if_valid_parenthesis(s):
    stack = []
    for c in s:
        if c in "({[":
            stack.append(c)
        else:
            try:
                if stack[-1] == PAIRS[c]:
                    stack.pop()
                else:
                    return False
            except IndexError:
                return False
    return len(stack) == 0

isValidParenthesis :: String -> Bool




main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = lines contents
    let outputs = map isValidParenthesis inputs
    mapM print outputs

