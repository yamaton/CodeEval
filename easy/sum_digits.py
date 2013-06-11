#!/usr/bin/env python
# encoding: utf-8
"""
sum_digits.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Given a positive integer, find the sum of its constituent digits.

Input sample:

The first argument will be a text file containing positive integers, one per line. e.g. 

23
496

Output sample:

Print to stdout, the sum of the numbers that make up the integer, one per line. e.g.

5
19

"""

import sys

def integerdigits(n):
    """
    Construct list of decimal digits from the integer n.
    """
    x = []
    quotient = n
    while quotient > 0:
        x.append(quotient % 10)
        quotient /= 10
    x.reverse()
    return x


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(x) for x in f]
        out = [sum(integerdigits(n)) for n in seq]
        print "\n".join(str(i) for i in out)


