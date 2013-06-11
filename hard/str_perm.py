#!/usr/bin/env python
# encoding: utf-8
"""
str_perm.py

Created by Yamato Matsuoka on 2012-07-18.

Description:

Write a program to print out all the permutations of a string in alphabetical order.

Input sample:

The first argument will be a text file containing an input string, one per line. e.g. 

hat

Output sample:

Print to stdout, permutations of the string, comma separated, in alphabetical order.
e.g.

aht,ath,hat,hta,tah,tha

"""

import sys
import itertools

def str_permutations(s):
    """string permutations"""
    x = itertools.permutations(s)
    return deleteduplicates(sorted("".join(i) for i in x))


def deleteduplicates(iterable):
    """Return iterator by remove duplicates"""
    seen = []
    for x in iterable:
        if x not in seen:
            seen.append(x)
            yield x


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [s.rstrip() for s in f]
    out = list(str_permutations(x) for x in seq)
    print "\n".join(",".join(i) for i in out)
