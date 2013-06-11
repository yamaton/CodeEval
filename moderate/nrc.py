#!/usr/bin/env python
# encoding: utf-8
"""
nrc.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Write a program to find the first non repeated character in a string.

Input sample:

The first argument will be a text file containing strings. e.g. 

yellow
tooth

Output sample:

Print to stdout, the first non repeating character, one per line.
e.g.

y
h

"""

import sys
import operator

def tally(lis):
    d = {}
    for i in lis:
        if d.has_key(i):
            d[i] += 1
        else:
            d[i] = 1
    out = d.items()
    out.sort(key=operator.itemgetter(-1))
    return out


def nonrepeated_character(s):
    singles = [i for (i,j) in tally(s) if j==1]
    for c in s:
        if c in singles:
            return c

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [s.rstrip() for s in f]
    
    out = (nonrepeated_character(s) for s in data)
    print "\n".join(out)
