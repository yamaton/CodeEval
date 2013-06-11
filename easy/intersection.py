#!/usr/bin/env python
# encoding: utf-8
"""
intersection.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

You are given two sorted list of numbers(ascending order). The lists themselves are comma delimited and the two lists are semicolon delimited. Print out the intersection of these two sets.

Input sample:

File containing two lists of ascending order sorted integers, comma delimited, one per line. e.g. 

1,2,3,4;4,5,6
7,8,9;8,9,10,11,12

Output sample:

Print out the ascending order sorted intersection of the two lists, one per line
e.g.

4
8,9
"""

import sys

def read_line(s):
    out = []
    for half in s.rstrip().split(";"):
        out.append([int(x) for x in half.split(",")])
    return out


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [read_line(s) for s in f]
    
    out = (set(i).intersection(j) for (i,j) in data)
    for entry in out:
        print ",".join(str(x) for x in sorted(entry))
    
