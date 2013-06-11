#!/usr/bin/env python
# encoding: utf-8
"""
happy_numbers.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

A happy number is defined by the following process. Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers.

Input sample:

The first argument is the pathname to a file which contains test data, one test case per line. Each line contains a positive integer. Each line is in the format: N i.e. a positive integer eg.

1
7
22

Output sample:

If the number is a happy number, print out a 1. If not, print out a 0 eg.

1
1
0

For the curious, here's why 7 is a happy number: 7->49->97->130->10->1. Here's why 22 is NOT a happy number:
 22->8->64->52->29->85->89->145->42->20->4->16->37->58->89 ..."""

import sys


def integerdigits(n):
    """
    Construct iterator of decimal digits from the integer n.
    The order is reversed but no problem for taking square sums.
    """
    x = []
    quotient = n
    while quotient > 0:
        yield (quotient % 10)
        quotient /= 10

def _digit_square_sum(n):
    return sum(i**2 for i in integerdigits(n))


def is_happy_number(n, seq):
    if n == 1:
        return 1
    elif n in seq:
        return 0
    else:
        return is_happy_number(_digit_square_sum(n), seq + [n])


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(x) for x in f]
    out = (is_happy_number(n,[]) for n in seq)
    
    print "\n".join(str(n) for n in out)

    