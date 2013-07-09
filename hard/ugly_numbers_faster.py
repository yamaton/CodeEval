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


Ver 1.10 @memo for is_ugly
"""

import sys
import itertools
import collections
import operator as op
import time
from functools import update_wrapper


def decorator(d):
    "Make function d a decorator: d wraps a function fn."
    def _d(fn):
        return update_wrapper(d(fn), fn)
    update_wrapper(_d, d)
    return _d


@decorator
def memo(f):
    """Decorator that caches the return value for each call to f(args).
    Then when called again with same args, we can just look it up."""
    cache = {}
    def _f(*args):
        try:
            return cache[args]
        except KeyError:
            cache[args] = result = f(*args)
            return result
        except TypeError:
            # some element of args can't be a dict key
            return f(args)
    return _f


def dotproduct(vec1, vec2, sum=sum, imap=itertools.imap, f=op.mul):
    """
    From itertools recipes
    http://docs.python.org/library/itertools.html
    """
    return sum(imap(f, vec1, vec2))


def roundrobin(*iterables):
    """
    roundrobin('ABC', 'D', 'EF') --> A D E B F C
    
    From itertools recipes
    http://docs.python.org/library/itertools.html
    """
    cycle = itertools.cycle
    islice = itertools.islice
    
    # Recipe credited to George Sakkis
    pending = len(iterables)
    nexts = cycle(iter(it).next for it in iterables)
    while pending:
        try:
            for next in nexts:
                yield next()
        except StopIteration:
            pending -= 1
            nexts = cycle(islice(nexts, pending))


def tuples(iterable, n):
    """
    Gives a generator of all possible n-tuples of elements from list lis
    """
    return itertools.product(*(itertools.repeat(iterable, n)))


@memo
def is_ugly(n):
    """Number is ugly if it's divisible by any of one-digit primes 
    i.e. (2,3,5,7)
    """
    return any((n % i) == 0 for i in (2,3,5,7))


def concat_combinations(string_seq):
    """
    Return integer list as a generator.
    
    concat_combinations("123")
    ==> ([123], [12, 3], [1, 23], [1, 2, 3])
    """
    n = len(string_seq)
    for seps in tuples(["", " "], n-1):
        tmp = "".join(roundrobin(string_seq, seps)).split()
        yield tuple(int(i) for i in tmp)


def _count_ugly(x):
    """
    rather than inserting + or - between integers, 
    multiply (+1) or (-1) each element and take the sum.
    """
    n = len(x)
    head = x[0]
    rest = x[1:]
    result = (head + dotproduct(rest, signs) for signs in tuples((1,-1), n-1))
    
    map = itertools.imap
    return sum(map(is_ugly, result))


def count_ugly_numbers(seq):
    return sum(_count_ugly(s) for s in concat_combinations(seq))


def test():
    assert list(roundrobin('abc','def', 'g', 'hi')) == ['a', 'd', 'g', 'h', 'b', 'e', 'i', 'c', 'f']
    assert list(roundrobin('12345','abcd')) == ['1', 'a', '2', 'b', '3', 'c', '4', 'd', '5']

    assert list(tuples(range(1,4), 2)) == [(1, 1), (1, 2), (1, 3), 
                   (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)]

    assert is_ugly(236) is True
    assert is_ugly(71) is False
    assert is_ugly(0) is True
    assert is_ugly(1) is False

    assert count_ugly_numbers("1") == 0 
    assert count_ugly_numbers("9") == 1
    assert count_ugly_numbers("011") == 6
    assert count_ugly_numbers("12345") == 64
    
    t0 = time.time()
    ## regression test
    assert count_ugly_numbers("062961902") == 4686
    assert count_ugly_numbers("02180737102") == 45819
    assert count_ugly_numbers("0218073710232") == 409956
    t1 = time.time()
    print "time =", t1 - t0
    print "passed all tests!"
    
    
if __name__ == '__main__':
    test()
    # 
    # with open(sys.argv[1], "r") as f:
    #     data = [s.rstrip() for s in f if s.rstrip()]
    # out = (count_ugly_numbers(s) for s in data)
    # for i in out:
    #     print i

