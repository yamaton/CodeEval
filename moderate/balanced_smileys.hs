{-
Created by Yamato Matsuoka on 2013-07-03

Balanced Smileys
=================
Description
------------
Credits: This problem appeared in the Facebook Hacker Cup 2013 Hackathon.

Your friend John uses a lot of emoticons when you talk to him on Messenger. In addition to being a person who likes to express himself through emoticons, he hates unbalanced parenthesis so much that it makes him go :(.

Sometimes he puts emoticons within parentheses, and you find it hard to tell if a parenthesis really is a parenthesis or part of an emoticon. A message has balanced parentheses if it consists of one of the following:

- An empty string ""
- One or more of the following characters: 'a' to 'z', ' ' (a space) or ':' (a colon)
- An open parenthesis '(', followed by a message with balanced parentheses, followed by a close parenthesis ')'.
- A message with balanced parentheses followed by another message with balanced parentheses.
- A smiley face ":)" or a frowny face ":("

Write a program that determines if there is a way to interpret his message while leaving the parentheses balanced.

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a message that you got from John. e.g.
```
:((
i am sick today (:()
(:)
hacker cup: started :):)
)(
```

Output sample
--------------
Print out the string "YES"/"NO"(all quotes for clarity only) stating whether or not it is possible that the message had balanced parentheses. e.g.
```
NO
YES
YES
YES
NO
```

-}

import System.Environment (getArgs)
import Data.Text (pack, unpack, count, splitOn, append, Text)
import Control.Applicative ((<$>), (<*>))

isBalanced :: String -> Bool
isBalanced "" = True
isBalanced s
  | any (`notElem` allowed) s = False
  | otherwise                 = any isBalancedProperly (realizations (pack s))
    where 
      allowed = ['a' .. 'z'] ++ "(): "


realizations :: Text -> [Text] 
realizations text = concatMap (\(x:xs) -> foldl (\acc txt -> append <$> acc <*> [txt, frown `append` txt]) [x] xs) xxs
  where 
    [smile, frown] = map pack [":)", ":("]
    (y:ys) = splitOn smile text :: [Text]
    textList  = foldl (\acc txt -> append <$> acc <*> [txt, smile `append` txt]) [y] ys :: [Text]
    xxs = map (splitOn frown) textList


isBalancedProperly :: Text -> Bool
isBalancedProperly text = isAllowed && isBalanced
  where 
    helper :: Int -> Char -> Int
    helper acc '(' = acc + 1
    helper acc ')' = acc - 1
    helper acc _   = acc
    s = unpack text    
    stack = scanl helper 0 s :: [Int]
    isBalanced = last stack == 0
    isAllowed  = all (>= 0) stack

boolToYesNo :: Bool -> String
boolToYesNo True  = "YES"
boolToYesNo False = "NO"


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = lines contents
  let outputs = map (boolToYesNo . isBalanced) inputs
  --print $ realizations $ pack (head inputs)
  mapM_ putStrLn outputs
