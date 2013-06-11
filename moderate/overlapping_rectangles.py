#!/usr/bin/env python
# encoding: utf-8
"""
Description:

Given two axis aligned rectangles A and B, determine if the two overlap.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains 8 comma separated co-ordinates. The co-ordinates are upper left x of A, upper left y of A, lower right x of A, lower right y of A, upper left x of B, upper left y of B, lower right x of B, lower right y of B. e.g.

-3,3,-1,1,1,-1,3,-3
-3,3,-1,1,-2,4,2,2

Output sample:

Print out True or False if A and B intersect. e.g.

False
True

"""
import sys

def if_intersect(x1min, y1max, x1max, y1min, x2min, y2max, x2max, y2min):
    return (x2min - x1max) * (x2max - x1min) < 0 and (y2min - y1max) * (y2max - y1min) < 0

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [[int(x) for x in line.rstrip().split(',')] for line in f]
    for seq in data:
        print if_intersect(*seq)

