#!/usr/bin/env python
# encoding: utf-8
"""
dupes.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

You are given a sorted list of numbers with duplicates. Print out the sorted list with duplicates removed.

Input sample:

File containing a list of sorted integers, comma delimited, one per line. e.g. 

1,1,1,2,2,3,3,4,4
2,3,4,5,5

Output sample:

Print out the sorted list with duplicates removed, one per line
e.g.

1,2,3,4
2,3,4,5
"""

import sys

def deleteduplicates(iterable):
    """Return iterator by remove duplicates"""
    seen = []
    for x in iterable:
        if x not in seen:
            yield x
            seen.append(x)


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split(",")] for line in f]
    
    out = (deleteduplicates(seq) for seq in data)
    print "\n".join( ",".join(str(x) for x in line) for line in out )

