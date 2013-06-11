#!/usr/bin/env python
# encoding: utf-8
"""
multiples.py

Created by Yamato Matsuoka on 2012-07-16.

Description: Given numbers x and n, where n is a power of 2, print out the smallest multiple of n which is greater than or equal to x. Do not use division or modulo operator.

Input sample:

The first argument will be a text file containing a comma separated list of two integers, one list per line. e.g. 

13,8
17,16

Output sample:

Print to stdout, the smallest multiple of n which is greater than or equal to x, one per line.
e.g.

16
32

"""

import sys
import itertools

def smallest_multiple(x, n):
    """
    x is an integer. n is a power of 2.
    Return the smallest multiple of n which is greater than or equal to x.
    """
    for i in itertools.count(1):
        multiple = i * n
        if multiple >= x:
            return multiple


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split(",")] for line in f]

    for (x, n) in data:
        print smallest_multiple(x,n)


        