"""
Decode Numbers

Description:

You are given an encoded message containing only numbers. You are also provided with the following mapping:

A : 1
B : 2
C : 3
...
Z : 26
Given an encoded message, count the number of ways it can be decoded.
Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is a testcase and contains an encoded message of numbers. e.g.

12
123
You may assume that the test cases contain only numbers.
Output sample:

Print out the different number of ways it can be decoded. e.g.

2
3
"""
import sys

def k(s):
    if len(s) == 0:
        return 1
    elif len(s) == 1:
        return 1
    else:
        if (0 < int(s[:2]) < 27):
            return k(s[1:]) + k(s[2:])
        else:
            return k(s[1:])

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [s.rstrip() for s in f]
    for s in data:
        print k(s)