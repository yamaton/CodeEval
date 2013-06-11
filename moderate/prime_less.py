#!/usr/bin/env python
# encoding: utf-8
"""
double_squares.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Print out the prime numbers less than a given number N. For bonus points your solution should run in N*(log(N)) time or better. You may assume that N is always a positive integer.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 4,294,967,295. eg.

10
20
100

Output sample:

For each line of input, print out the prime numbers less than N, in ascending order, comma delimited. (There should not be any spaces between the comma and numbers) eg.

2,3,5,7
2,3,5,7,11,13,17,19
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97

"""

import sys
import math


def EratosthenesSieve(N):
    """Construct a list of primes equal or less than N."""
    numbers = [True] * (N+1)
    max_p = int(math.sqrt(N))
    for p in (i for i in range(2, max_p+1) if numbers[i]):
        for q in range(p*p, N+1, p):
            numbers[q] = False
    return [i for i in range(2, N+1) if numbers[i]]


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(x) for x in f]
    
    out = (EratosthenesSieve(n) for n in seq)
    print "\n".join(",".join(str(i) for i in x) for x in out)
