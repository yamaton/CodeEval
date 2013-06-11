#!/usr/bin/env python
# encoding: utf-8
"""
number_pairs.py

Created by Yamato Matsuoka on 2012-07-17.

Description:

You are given a sorted array of positive integers and a number 'X'. Print out all pairs of numbers whose sum is equal to X. Print out only unique pairs and the pairs should be in ascending order

Input sample:

Your program should accept as its first argument a filename. This file will contain a comma separated list of sorted numbers and then the sum 'X', separated by semicolon. Ignore all empty lines. If no pair exists, print the string NULL eg.

1,2,3,4,6;5
2,4,5,6,9,11,15;20
1,2,3,4;50

Output sample:

Print out the pairs of numbers that equal to the sum X. The pairs should themselves be printed in sorted order i.e the first number of each pair should be in ascending order .e.g.

1,4;2,3
5,15;9,11
NULL

"""

import sys

def read_data(line):
    x = line.rstrip().split(";")
    seq = [int(i) for i in x[0].split(",")]
    N = int(x[-1])
    return (seq, N)



def find_pairs(seq, X):
    """Find number pairs from a sorted list, seq, 
    such that sum of each pair is X."""
    bag1 = [i for i in seq if i <= X/2]
    bag2 = [i for i in seq if i > X/2]
    
    out = []
    for i in bag1:
        j = X - i
        if j in bag2:
            out.append((i,j))
    return out


def format(lis):
    """Construct formatted string from a list of pairs"""
    if lis:
        return ";".join(",".join(str(i) for i in n) for n in lis)
    else:
        return "NULL"



if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [read_data(line) for line in f]
    
    out = (find_pairs(seq, X) for (seq, X) in data)
    formatted = "\n".join(format(x) for x in out)
    print formatted

