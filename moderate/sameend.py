#!/usr/bin/env python
# encoding: utf-8
"""
sameend.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

You are given two strings 'A' and 'B'. Print out a 1 if string 'B' occurs at the end of string 'A'. Else a zero.

Input sample:

The first argument is a file, containing two strings, comma delimited, one per line. Ignore all empty lines in the input file.e.g. 

Hello World,World
Hello CodeEval,CodeEval
San Francisco,San Jose

Output sample:

Print out 1 if the second string occurs at the end of the first string. Else zero. Do NOT print out empty lines between your output. 
e.g.

1
1
0

"""

import sys

# def is_sameend(entry):
#     """Return 1 if the last word of the first string is the same 
#     as the first word of the second string. 0 otherwise."""
#     first, second = entry
#     return first.split()[-1] == second.split()[0]

def is_sameend(entry):
    """Return 1 if the last word of the first string is the same 
    as the first word of the second string. 0 otherwise."""
    first, second = entry
    lenB = len(second)
    if len(first) < lenB:
        return False
    else:
        return first[-lenB:] == second


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[s for s in line.rstrip().split(",")] for line in f if line.rstrip()]
    
    out = (int(is_sameend(x)) for x in data)
    print "\n".join(str(n) for n in out)


