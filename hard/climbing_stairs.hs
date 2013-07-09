{-
Climbing Stairs
===============
Description
------------
You are climbing a stair case. It takes n steps to reach to the top. Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line in this file contains a positive integer which is the total number of stairs. e.g.
```
10
20
```

Output sample
--------------
Print out the number of ways to climb to the top of the staircase. e.g.
```
89
10946
```
-}
import System.Environment (getArgs)
import Control.Monad (forM)
import Control.Monad.State (evalState, get, put)

fibonacci :: Int -> Int
fibonacci n = flip evalState (0,1) $ do
  forM [0 .. (n-1)] $ \_ -> do
    (a, b) <- get
    put (b, a + b)
  (a, b) <- get
  return a

numberOfWays :: Int -> Int
numberOfWays n = fibonacci (n+1)


main = do 
  args <- getArgs
  contents <- readFile (head args)
  let inputs = map read $ lines contents
  let outputs = map numberOfWays inputs
  mapM print outputs

