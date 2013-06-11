#!/usr/bin/env python
# encoding: utf-8
"""
Armstrong Number

Description:

An Armstrong number is an n-digit number that is equal to the sum of the n'th powers of its digits. Determine if the input numbers are Armstrong numbers.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file has a positive integer. e.g.

6
153
351

Output sample:

Print out True/False if the number is an Armstrong number or not e.g.

True
True
False

"""

import sys

def integerdigits(n):
    """
    Construct list of decimal digits from the integer n
    """
    if n == 0: return [0]
    x = []
    quotient = n
    while quotient > 0:
        x.append(quotient % 10)
        quotient /= 10
    x.reverse()
    return x


def is_armstrong(x):
    digits = integerdigits(x)
    n = len(digits)
    return x == sum(i ** n for i in digits)

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [int(x) for x in f]

    for x in data:
        print is_armstrong(x)
