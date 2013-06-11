#!/usr/bin/env python
# encoding: utf-8
"""
ugly_numbers.py

Created by Yamato Matsuoka on 2012-07-18.

Description:

Credits: This challenge has appeared in a google competition before.

Once upon a time in a strange situation, people called a number ugly if it was divisible by any of the one-digit primes (2, 3, 5 or 7). Thus, 14 is ugly, but 13 is fine. 39 is ugly, but 121 is not. Note that 0 is ugly. Also note that negative numbers can also be ugly; -14 and -39 are examples of such numbers.

One day on your free time, you are gazing at a string of digits, something like:

123456

You are amused by how many possibilities there are if you are allowed to insert plus or minus signs between the digits. For example you can make:

1 + 234 - 5 + 6 = 236

which is ugly. Or

123 + 4 - 56 = 71

which is not ugly.

It is easy to count the number of different ways you can play with the digits: Between each two adjacent digits you may choose put a plus sign, a minus sign, or nothing. Therefore, if you start with D digits there are 3^(D-1) expressions you can make. Note that it is fine to have leading zeros for a number. If the string is '01023', then '01023', '0+1-02+3' and '01-023' are legal expressions.

Your task is simple: Among the 3^(D-1) expressions, count how many of them evaluate to an ugly number.


Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will be a single line containing a non-empty string of decimal digits. The string in each test case will be non-empty and will contain only characters '0' through '9'. Each string is no more than 13 characters long. eg.

1
9
011
12345

Output sample:

Print out the number of expressions that evaluate to an ugly number for each test case, each one on a new line eg

0
1
6
64

"""

import sys
import operator as op
import itertools
import re


def tuples(lis,n):
    """
    Generates a list of all possible n-tuples of elements from list lis
    """
    out = []
    for i in range(n):
        out.append(lis)
    return (x for x in itertools.product(*out))


def is_ugly(n):
    """Number is ugly if it's divisible by any of one-digit primes 
    i.e. (2,3,5,7)
    """
    return any((n % i) == 0 for i in (2,3,5,7))


def riffle(iter1, iter2):
    """lis1 and lis2 must have the same length"""
    return (i for j in itertools.izip(iter1, iter2) for i in j)


def count_ugly_numbers(s):
    n = len(s) 
    symbols = tuples(["", "+", "-"], n-1)
    symbols = (itertools.chain(i,[""]) for i in symbols)
    out = (eval(cleanup("".join(riffle(s,symb)))) for symb in symbols)
    
    # debug = sorted([i for i in out if is_ugly(i)])
    # print debug
    # print ''
    
    return sum(is_ugly(i) for i in out)


def cleanup(s):
    """fix expressions like '0135+03' to '135+3'"""
    s = re.sub("^0+([1-9])",  "\\1", s)
    s = re.sub("\+0+([1-9])", "+\\1", s)
    s = re.sub("\-0+([1-9])", "-\\1", s)
    return s


if __name__ == '__main__':
    # with open(sys.argv[1], "r") as f:
    #     data = [s.rstrip() for s in f if s.rstrip()]
    # 
    # out = (count_ugly_numbers(s) for s in data)
    # print "\n".join(str(i) for i in out)

    print count_ugly_numbers("02180737102")
    # print count_ugly_numbers("12345")
