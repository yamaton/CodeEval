#!/usr/bin/env python
# encoding: utf-8
"""
largest_sum.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Write a program to determine the largest sum of contiguous integers in a list.

Input sample:

The first argument will be a text file containing a comma separated list of integers, one per line. e.g. 

-10, 2, 3, -2, 0, 5, -15
2,3,-2,-1,10

Output sample:

Print to stdout, the largest sum. In other words, of all the possible contiguous subarrays for a given array, find the one with the largest sum, and print that sum.
e.g.

8
12

"""

import sys

def contiguous_sum_max(lis):
    idx_max = len(lis)
    candid = []
    for i in range(idx_max):
        for j in range(i+1, idx_max+1):
            candid.append(sum(lis[i:j]))
    return max(candid)


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split(",")] for line in f]
    
    out = (contiguous_sum_max(x) for x in data)
    print "\n".join(str(x) for x in out)

