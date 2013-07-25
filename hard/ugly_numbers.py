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
import itertools
import collections
import operator as op

def roundrobin(*iterables):
    """
    roundrobin('ABC', 'D', 'EF') --> A D E B F C
    
    ** From itertools recipes **
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
        yield [int(i) for i in tmp]


def _count_ugly(seq):
    """
    use prefix notation to evaluate.
    """
    N = len(seq)
    result = []
    for operators in tuples((op.add, op.sub), N-1):
        ## Deque is faster than list given by "stack = s[:]"
        stack = collections.deque(seq) 
        for f in operators:
            x1 = stack.pop()
            x2 = stack.pop()
            stack.append(f(x1, x2))
        result.append(stack[0])
    return sum(is_ugly(i) for i in result)


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
    print "passed all tests!"

if __name__ == '__main__':
    # test()
    with open(sys.argv[1], "r") as f:
        data = [s.rstrip() for s in f if s.rstrip()]
    out = (count_ugly_numbers(s) for s in data)
    print "\n".join(str(i) for i in out)
