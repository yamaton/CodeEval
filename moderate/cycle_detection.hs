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

def detect_cycle(seq):
    """
    Crude algorithm to find a cycle such that
    
    - Select the subsequence seq[n:] such that n is the minimum.
    - Return 1 cycle orbit of the subsequence.
    """
    for n in range(len(seq)):
        x = seq[n:]
        period = periodicity_len(x)
        if period > 0:
            return x[:period]
    else:
        return False


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
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let inputs = [map read (words line) | line <- lines contents]
    let outputs = filter (not . null) $ map findCycle inputs
    mapM putStrLn $ [unword (map show out) | out <- outputs] 

