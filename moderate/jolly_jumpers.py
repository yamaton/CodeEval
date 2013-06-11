#!/usr/bin/env python
# encoding: utf-8
"""
jolly_jumpers.py

Created by Yamato Matsuoka on 2012-07-17.

Description:

Credits: Programming Challenges by Steven S. Skiena and Miguel A. Revilla

A sequence of n > 0 integers is called a jolly jumper if the absolute values of the differences between successive elements take on all possible values 1 through n - 1. eg.

1 4 2 3

is a jolly jumper, because the absolute differences are 3, 2, and 1, respectively. The definition implies that any sequence of a single integer is a jolly jumper. Write a program to determine whether each of a number of sequences is a jolly jumper.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 3000 followed by n integers representing the sequence. The integers are space delimited.

4 1 4 2 3
5 1 4 2 -1 6

Output sample:

For each line of input generate a line of output saying 'Jolly' or 'Not jolly'.

Jolly
Not jolly

"""

import sys

def is_jolly_jumber(seq):
    n = len(seq)
    if n == 1:
        return True
    else:
        return range(1,n) == sorted(abs(seq[i+1]-seq[i]) for i in range(n-1))


## previous version
#
# def is_jolly_jumber(seq):
#     x = len(seq)
#     if x == 1:
#         return True
#     elif x < 1:
#         return False
#     else:
#         if set(abs(seq[i+1]-seq[i]) for i in range(x-1)) == set(range(1,x)):
#             return True
#         else:
#             return False


def jolly_message(tf):
    if tf:
        return "Jolly"
    else:
        return "Not jolly"


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split()] for line in f if line.rstrip()]
        data = [series[1:] for series in data]
    
    out = (is_jolly_jumber(seq) for seq in data)
    print "\n".join(jolly_message(x) for x in out)
