"""
Sum to Zero  Share on LinkedIn

Description:

You are given an array of integers. Count the numbers of ways in which the sum of 4 elements in this array results in zero

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file consist of comma separated positive and negative integers. e.g.

2,3,1,0,-4,-1
0,-1,3,-2

Output sample:

Print out the count of the different number of ways that 4 elements sum to zero. e.g.

2
1
"""
import sys
import itertools
import math

def count_sum4(seq):
    return sum(sum(subset) == 0 for subset in itertools.combinations(seq, 4))

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [[int(x) for x in line.rstrip().split(',')] for line in f]
    for seq in data:
        print count_sum4(seq)