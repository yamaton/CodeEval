#!/usr/bin/env python
# encoding: utf-8
"""
int_palindrome.py


Problem: Palindromic Ranges
===========================

Description:
------------

A positive integer is a palindrome if its decimal representation (without leading zeros) is a palindromic string (a string that reads the same forwards and backwards). For example, the numbers 5, 77, 363, 4884, 11111, 12121 and 349943 are palindromes.

A range of integers is interesting if it contains an even number of palindromes. The range [L, R], with L <= R, is defined as the sequence of integers from L to R (inclusive): (L, L+1, L+2, ..., R-1, R). L and R are the range's first and last numbers.

The range [L1,R1] is a subrange of [L,R] if L <= L1 <= R1 <= R. Your job is to determine how many interesting subranges of [L,R] there are.

Input sample:
-------------

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain two positive integers, L and R (in that order), separated by a space. eg.

1 2
1 7

Output sample:
--------------

For each line of input, print out the number of interesting subranges of [L,R] eg.

1
12

Submit your solution in a file (some file name).(py| c| cpp| rb| pl| php| tcl| clj| js) | int_palindrome.java or use the online editor.

"""

import sys
import os

def palindromicQ(n):
    """Return if the integer n is palindromic or not."""
    s = str(n)
    return s == s[::-1]
    
def interestingQ(r):
    """
    Return if the range r of integers is 'interesting' by checking 
    if the number of  palindrome is even in the range.
    """
    palindromes = [i for i in r if palindromicQ(i)]
    return len(palindromes)%2==0

def interesting_surbranges(r):
    """
    Deterine number of interesting subranges of the given range r.
    """
    subranges = partitions(r)
    truefalse = [interestingQ(x) for x in subranges]    
    return truefalse.count(True)

def partitions(r):
    """
    Return all-possible subranges of the given range r
    """
    n = len(r)
    if n < 2:
        sys.exit('partitions: invalid range r')

    out = []
    for width in range(2, n+1):
        for i in range(n-width+1):
            out.append(r[i:i+width])
    return out

    
def main(argv=None):
    if argv is None:
        argv = sys.argv
    
    if len(sys.argv) < 2:
        sys.exit('Usage: Give input data as the argument')
        
    try:
        f = open(sys.argv[1], "r")
    except IOError:
        print "cannot open", argv[1]
            
    data = [s.split() for s in f.readlines()]
    f.close()
    
    data = [range(int(pair[0]), int(pair[1])+1) for pair in data]
    for r in data:
        print interesting_surbranges(r)

        
if __name__ == '__main__':
    main()

