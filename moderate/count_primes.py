#!/usr/bin/env python
# encode: utf-8
"""
Counting Primes
Created by Yamato Matsuoka on 2013-02-05

Description:

Given two integers N and M, count the number of prime numbers between N and M (both inclusive)

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated positive integers. e.g.

2,10
20,30

Output sample:

Print out the number of primes between N and M (both inclusive)

"""
import sys
import math

def EratosthenesSieve(N):
    """Construct a list of primes equal or less than N."""
    if N < 2:
        return []
    numbers = [True] * (N + 1)
    max_p = int(math.sqrt(N))
    for p in (i for i in range(2, max_p+1) if numbers[i]):
        for q in range(p*p, N+1, p):
            numbers[q] = False
    return [i for i in range(2, N+1) if numbers[i]]


def primePi(n):
    """
    Count number of primes equal or less than n.
    """
    return len(EratosthenesSieve(n))


def count_primes(a, b):
    return primePi(b) - primePi(a-1)


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [[int(x) for x in line.rstrip().split(',')] for line in f]

    for (a, b) in data:
        print count_primes(a, b)
