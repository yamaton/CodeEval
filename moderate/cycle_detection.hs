{-
cycle_detection.hs

Created by Yamato Matsuoka on 2013-07-04.

Description
------------
Given a sequence, write a program to detect cycles within it.

Input sample
-------------
A file containing a sequence of numbers (space delimited). The file can have multiple such lines. e.g
```
2 0 6 3 1 6 3 1 6 3 1
```
Ensure to account for numbers that have more than one digit eg. 12. If there is no sequence, ignore that line.

Output sample
--------------
Print to stdout the first sequence you find in each line. Ensure that there are no trailing empty spaces on each line you print. e.g.
```
6 3 1
```

-}

import System.Environment (getArgs)
import Data.List (tails)
import Data.Maybe (isNothing, fromJust)
import Data.Control.Applicative ((<$>), (<*>)) 

detectCycle :: Eq a => [a] -> Maybe [a]
detectCycle xs = take <$> (periodicityLength <$> zs) <*> zs
  where zs = find (\ys -> periodicityLength ys == 0) (tails xs)


-- return 0 if xs is aperiodic
periodicityLength :: Eq a => [a] -> Int
periodicityLength x:xs = 
  where
    len = length xs
    helper acc zs =     



def periodicity_len(seq):
    """
    Return length of periodic orbit. seq must NOT contain transient.
    0 is returned if seq is aperiodic.
    """
    N = len(seq)
    head = seq[0]
    
    for i in range(1, N/2):
        try:
            periodicity = i + seq[i:].index(head) 
        except ValueError:
            continue
        if periodicity <= N/2:
            if all( seq[k+periodicity] == seq[k] for k in range(N-periodicity) ):
                return periodicity
    else:
        return 0


main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs = [map read (words line) | line <- filter (not . null) (lines contents)]
    let outputs = filter (not . null) $ map findCycle inputs
    mapM_ putStrLn $ [unword (map show out) | out <- outputs] 
