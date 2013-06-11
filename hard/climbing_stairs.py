"""
Climbing Stairs  Share on LinkedIn

Description:

You are climbing a stair case. It takes n steps to reach to the top. Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains a positive integer which is the total number of stairs. e.g.

10
20

Output sample:

Print out the number of ways to climb to the top of the staircase. e.g.

89
10946
"""
import sys
import math

def fibonacci(n):
    seq = [0, 1]
    for i in range(2,n+1):
        seq.append(sum(seq[-2:]))
    return seq[-1]


def number_of_ways(n):
    return fibonacci(n+1)

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [int(x) for x in f]
    for n in data:
        print number_of_ways(n)