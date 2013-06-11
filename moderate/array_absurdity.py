#!/usr/bin/env python
# encoding: utf-8
"""
array_absurdity.py

Created by Yamato Matsuoka on 2012-07-17.

Description:

Imagine we have an immutable array of size N which we know to be filled with integers ranging from 0 to N-2, inclusive. Suppose we know that the array contains exactly one duplicated entry and that duplicate appears exactly twice. Find the duplicated entry. (For bonus points, ensure your solution has constant space and time proportional to N)

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Ignore all empty lines. Each line begins with a positive integer(N) i.e. the size of the array, then a semicolon followed by a comma separated list of positive numbers ranging from 0 to N-2, inclusive. i.e eg.

5;0,1,2,3,0
20;0,1,10,3,2,4,5,7,6,8,11,9,15,12,13,4,16,18,17,14

Output sample:

Print out the duplicated entry, each one on a new line eg

0
4

"""

import sys

def read_data(seq):
    x = seq.split(";")
    N = int(x[0])
    seq = [int(i) for i in x[1].split(",")]
    return (N, seq)


# def find_duplicates(N, seq):
#     d = {}
#     for i in seq:
#         if d.has_key(i):
#             return i
#         else:
#             d[i] = 1


def find_duplicates(N, seq):
    return sum(seq) - (N-2)*(N-1)/2


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = (line.rstrip() for line in f)
        data = (read_data(seq) for seq in data if seq)
        data = [x for x in data if x]
    
    out = [find_duplicates(N, seq) for (N, seq) in data]
    print "\n".join(str(n) for n in out)
