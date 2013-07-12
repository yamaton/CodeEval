{-
str_substitution.hs

Created by Yamato Matsuoka on 2013-07-11.

Description
------------
Credits: This challenge was contributed by Sam McCoy

Given a string S, and a list of strings of positive length, F1,R1,F2,R2,...,FN,RN, proceed to find in order the occurrences (left-to-right) of Fi in S and replace them with Ri. All strings are over alphabet { 0, 1 }. Searching should consider only contiguous pieces of S that have not been subject to replacements on prior iterations. An iteration of the algorithm should not write over any previous replacement by the algorithm.

Input sample
------------
Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain a string, then a semicolon and then a list of comma separated strings.eg.
```
10011011001;0110,1001,1001,0,10,11
```

Output sample
-------------
For each line of input, print out the string after substitutions have been made.eg.
```
11100110
```

For the curious, here are the transitions for the above example: 10011011001 => 10100111001 [replacing 0110 with 1001] => 10100110 [replacing 1001 with 0] => 11100110 [replacing 10 with 11] => 11100110


Ver 0.13
Fixed offset variable by replacing "offset =" by "offset +=" 

Ver 0.12  
Fixed the bug of matched string position by adding offset variable.
Added nice printing for debuggging. Still fails.

Ver 0.11 
Fixed regular expression and corresponding codes to match overlapping items.

Ver 0.10 
First attempt failed.
-}

import System.Environment (getArgs)

def reshape(seq, (row, col)):
    """
    This is numpy's reshape equivalent.
    row * col == len(seq) must be satisfied.
    """
    if len(seq) != row * col:
        raise ValueError("row and column size are invalid.")
    out = []
    for i in range(row):
        x = seq[i*col:(i+1)*col]
        out.append(x)
    return out


def search_and_replace(s, openQ, patt, repl):
    patt_regex = "(?=%s)" % patt
    offset = 0  
    regions = [m.start() for m in re.finditer(patt_regex, s)]
    for start in regions:
        start += offset  # Necessary because positions are messed up
        end = start + len(patt)
        ## [for debugging] comment out these 
        # print "".join("." if i else "x" for i in openQ)
        # print s
        # print " "*start + patt + " --> " + repl
        # print ""
        ## Make sure matched region still matchs with the pattern
        if all(openQ[start:end]) and s[start:end] == patt:  
            s = s[:start] + repl + s[end:]
            openQ = openQ[:start] + [False]*len(repl) + openQ[end:]
            offset += len(repl) - len(patt)
    return (s, openQ)


def str_substitute(s, patt_repl):
    openQ = [True] * len(s)
    for (patt, repl) in patt_repl:
        s, openQ = search_and_replace(s, openQ, patt, repl)
    return s


def read_data(line):
    s, patt_repl = line.rstrip().split(";")
    patt_repl = patt_repl.split(",")
    patt_repl = reshape(patt_repl, (len(patt_repl)/2, 2))
    return (s, patt_repl)


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [read_data(s) for s in f if s.rstrip()]
    out = (str_substitute(s, patt_repl) for (s,patt_repl) in data)
    print "\n".join(out)
