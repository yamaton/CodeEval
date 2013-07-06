{-
pass_triangle.hs

Challenge Description
======================

By starting at the top of the triangle and moving to adjacent 
numbers on the row below, the maximum total from top to bottom is 27.
```
   5
  9 6
 4 6 8
0 7 1 5
```
5 + 9 + 6 + 7 = 27

## Input sample
Your program should accept as its first argument a path to a filename. 
Input example is the following
```
5
9 6
4 6 8
0 7 1 5
```
You make also check full input file which will be used for your code evaluation.

## Output sample
The correct output is the maximum sum for the triangle. 
So for the given example the correct answer would be
```
27
```
-}

import System.Environment (getArgs)

def add_neighbour_max(sec1, sec2):
    reduction = map(max, zip(sec1[:-1], sec1[1:]))
    return [x + y for (x,y) in zip(sec2, reduction)]

def max_triangle_path(triangle):
    out = reduce(add_neighbour_max, reversed(triangle))
    return out[0]

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in s.split()] for s in f]
    
    print max_triangle_path(data)
