{-
point_in_circle.hs

## Challenge Description
Having coordinates of the center of a circle, it's radius and coordinates 
of a point you need to define whether this point is located inside of this circle.

## Input sample
Your program should accept as its first argument a path to a filename. 
Input example is the following
```
Center: (2.12, -3.48); Radius: 17.22; Point: (16.21, -5)
Center: (5.05, -11); Radius: 21.2; Point: (-31, -45)
Center: (-9.86, 1.95); Radius: 47.28; Point: (6.03, -6.42)
```

All numbers in input are between -100 and 100

## Output sample
Print results in the following way.
```
true
false
true
```
-}

import System.Environment (getArgs)
import Text.Regex.Posix


reader :: String -> (Int, Int, Int, Int, Int)
reader s =  
    where  pattern = "Center: +\\((-?[.0-9]+), +(-?[.0-9]+)\\); +Radius: +(-?[.0-9]+); +Point: +\\((-?[.0-9]+), (-?[.0-9]+)\\)"
           matching = s' =~ pattern

def reader(s):
    result = regex.search(s)
    (x, y, r, px, py) = map(float, result.groups())
    return (x, y, r, px, py)


def is_point_in_circle(x, y, r, px, py):
    dist_squared = (x - px)**2 + (y - py)**2
    return "true" if dist_squared < r*r else "false"


boolToString :: Bool -> String 
boolToString True  = "true"
boolToString False = "false"

main = do 
    args <- getArgs
    contents <- readFile (head args)
    let input = 

    out = [is_point_in_circle(x, y, r, px, py) for (x, y, r, px, py) in data]
    for x in out:
        print x
