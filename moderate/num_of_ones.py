#!/usr/bin/env python
# encoding: utf-8
"""
num_of_ones.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Write a program to determine the number of 1 bits in the internal representation of a given integer.

Input sample:

The first argument will be a text file containing an integer, one per line. e.g. 

10
22
56

Output sample:

Print to stdout, the number of ones in the binary form of each number.
e.g.

2
3
3

"""

import sys
import os


def num_of_one_bits(n):
    s = bin(n)
    return s.count('1')


if __name__ == '__main__':
	with open(sys.argv[1], "r") as f:
	    seq = [int(x) for x in f]
	
	out = (num_of_one_bits(n) for n in seq)
	print "\n".join(str(n) for n in out)


