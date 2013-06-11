#!/usr/bin/env python
# encoding: utf-8
"""
m2last.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Write a program to determine the Mth to last element of a list.

Input sample:

The first argument will be a text file containing a series of space delimited characters followed by an integer representing a index into the list(1 based), one per line. e.g. 

a b c d 4
e f g h 2

Output sample:

Print to stdout, the Mth element from the end of the list, one per line. If the index is larger than the list size, ignore that input. 
e.g.

a
g

"""

import sys

def m2last(entry):
    n = int(entry.pop())
    
    if len(entry) < n:
        return False
    else:
        return entry[-n]




if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[s for s in line.split()] for line in f]
        
    out = (m2last(x) for x in data)
    print "\n".join(str(x) for x in out if x)
