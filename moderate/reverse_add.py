#!/usr/bin/env python
# encoding: utf-8
"""
reverse_add.py

Created by Yamato Matsuoka on 2012-07-17.

Description:

Credits: Programming Challenges by Steven S. Skiena and Miguel A. Revilla

The problem is as follows: choose a number, reverse its digits and add it to the original. If the sum is not a palindrome (which means, it is not the same number from left to right and right to left), repeat this procedure. eg.

195 (initial number) + 591 (reverse of initial number) = 786

786 + 687 = 1473

1473 + 3741 = 5214

5214 + 4125 = 9339 (palindrome)

In this particular case the palindrome 9339 appeared after the 4th addition. This method leads to palindromes in a few step for almost all of the integers. But there are interesting exceptions. 196 is the first number for which no palindrome has been found. It is not proven though, that there is no such a palindrome.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 4,294,967,295. Assume each test case will always have an answer and that it is computable with less than 1000 iterations (additions)

Output sample:

For each line of input, generate a line of output which is the number of iterations (additions) to compute the palindrome and the resulting palindrome. (they should be on one line and separated by a single space character)

"""

import sys


def integerdigits(n):
    """
    Construct an iterator of decimal digits from the integer n
    """
    x = []
    quotient = n
    while quotient > 0:
        x.append(quotient % 10)
        quotient /= 10
    x.reverse()
    return x


def fromdigits(x):
    """
    Constructs an integer from the list x of decimal digits.
    
    Input: list of decimal digits 
    Output: integer
    """
    return reduce(lambda i,j: 10*i + j, x)


def is_palindrome(n):
    """Return True if integer n is palindrome."""
    x = integerdigits(n)
    return n == fromdigits(reversed(x))


def check_sanity(n):
    if (n == 196):
        raise ValueError("The chain won't converge.")
        sys.exit()


def reverse_add(n):
    return (n + fromdigits(reversed(integerdigits(n))))


def reverse_add_chain(x):
    seq = [x]
    check_sanity(x)
    
    while not is_palindrome(x):
        x = reverse_add(x)
        seq.append(x)
        check_sanity(x)
    return len(seq)-1, x
    


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(x) for x in f]
    
    out = (reverse_add_chain(x) for x in seq)
    print "\n".join(" ".join(str(i) for i in x) for x in out)
    
