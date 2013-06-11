#!/usr/bin/env python
# encoding: utf-8
"""
fibonacci.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

The Fibonacci series is defined as: F(0) = 0; F(1) = 1; F(n) = F(n-1) + F(n-2) when n>1;. Given a positive integer 'n', print out the F(n).

Input sample:

The first argument will be a text file containing a positive integer, one per line. e.g. 

5
12

Output sample:

Print to stdout, the fibonacci number, F(n). e.g.

5
144
"""

import sys
import math

def fibonacci(n):
    seq = [0, 1]
    for i in range(2,n+1):
        seq.append(sum(seq[-2:]))
    return seq[-1]




if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(i) for i in f]
        out = [fibonacci(n) for n in seq]
    print "\n".join(str(n) for n in out)
