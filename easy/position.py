#!/usr/bin/env python
# encoding: utf-8
"""
position.py

Created by Yamato Matsuoka on 2012-07-16.


Description:

Given a number n and two integers p1,p2 determine if the bits in position p1 and p2 are the same or not. Positions p1,p2 and 1 based.

Input sample:

The first argument will be a text file containing a comma separated list of 3 integers, one list per line. e.g. 

86,2,3
125,1,2

Output sample:

Print to stdout, 'true'(lowercase) if the bits are the same, else 'false'(lowercase).
e.g.

true
false
"""

import sys


def are_the_same_bits(lis):
    n, p1, p2 = lis
    x = bin(n)
    return x[-p1] == x[-p2]


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split(",")] for line in f]
    
    out = (are_the_same_bits(entry) for entry in data)
    out = (str(x).lower() for x in out)
    print "\n".join(out)


