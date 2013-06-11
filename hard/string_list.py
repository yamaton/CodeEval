#!/usr/bin/env python
# encoding: utf-8
"""
string_list.py

Created by Yamato Matsuoka on 2012-07-18.

Description:

Credits: Challenge contributed by Max Demian.

You are given a number N and a string S. Print all of the possible ways to write a string of length N from the characters in string S, comma delimited in alphabetical order.


Input sample:

The first argument will be the path to the input filename containing the test data. Each line in this file is a separate test case. Each line is in the format: N,S i.e. a positive integer, followed by a string (comma separated) eg.

1,aa
2,ab
3,pop


Output sample:

Print all of the possible ways to write a string of length N from the characters in string S comma delimited in alphabetical order, with no duplicates. eg.

a
aa,ab,ba,bb
ooo,oop,opo,opp,poo,pop,ppo,ppp

"""

import sys
import itertools

def deleteduplicates(iterable):
    """Return iterator by remove duplicates"""
    seen = []
    for x in iterable:
        if x not in seen:
            yield x
            seen.append(x)


def tuples(lis,n):
    lis = "".join(deleteduplicates(lis))
    out = []
    for i in range(n):
        out.append(lis)
    return sorted("".join(x) for x in itertools.product(*out))


def combinations(s, n):
    """Find all permutations of substring with length n from stirng s"""
    return ("".join(x) for x in tuples(s,n))


def read(entry):
    (N, string) = entry.rstrip().split(",")
    N = int(N)
    return (N, string)


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [read(x) for x in f if x.rstrip()]
    out = (combinations(string, N) for (N, string) in data)
    print "\n".join(",".join(x) for x in out)
