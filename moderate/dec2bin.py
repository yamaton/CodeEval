#!/usr/bin/env python
# encoding: utf-8
"""
dec2bin.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Given a decimal (base 10) number, print out its binary representation.

Input sample:

File containing positive whole decimal numbers, one per line. e.g. 

2
10
67

Output sample:

Print the decimal representation, one per line.
e.g.

10
1010
1000011

"""

import sys
import os


def dec2bin(n):
    return bin(n)[2:]

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(x) for x in f]
    out = (dec2bin(n) for n in seq)
    print "\n".join(out)


