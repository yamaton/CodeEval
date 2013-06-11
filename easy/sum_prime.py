#!/usr/bin/env python
# encoding: utf-8
"""
sum_prime.py

Created by Yamato Matsuoka on 2012-07-16.

Description: Write a program to determine the sum of the first 1000 prime numbers.

Input sample: None

Output sample:  Your program should print the sum on stdout.i.e.

"""

import math

def EratosthenesSieve(N):
    """
    Return iterator of primes less than or equal to N.
    """
    numbers = [True] * (N+1)
    max_p = int(math.sqrt(N))
    for p in (i for i in range(2, max_p+1) if numbers[i]):
        for q in range(p*p, N+1, p):
            numbers[q] = False
    return [i for i in range(2, N+1) if numbers[i]]
    


def prime(n):
    """
    Generate iterator of first n primes.
    
    Rosser's theorem is used to get an upper bound:
    For n-th prime number p(n), for n > 6
    log(n) + log(log(n)) - 1 < P(n)/n < log(n) + log(log(n))
    
    http://en.wikipedia.org/wiki/Prime_number_theorem    
    """
    log = math.log
    
    if n >= 6:
        upperbound = int(n * (log(n) + log(log(n))))
        out = EratosthenesSieve(upperbound)
    else:
        out = [2, 3, 5, 7, 11]
    
    return out[:n]



if __name__ == '__main__':
    n = 1000
    print sum(prime(n))

