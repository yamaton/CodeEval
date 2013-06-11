#!/usr/bin/env python
# encoding: utf-8
"""
String Rotation  Share on LinkedIn

Description:

You are given two strings. Determine if the second string is a rotation of the first string.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated strings. e.g.

Hello,lloHe
Basefont,tBasefon

Output sample:

Print out True/False if the second string is a rotation of the first. e.g.

True
True
"""
import sys


def if_string_rotation(s, t):
    if not len(s) == len(t):
        return False
    rotated = [s[i:] + s[:i] for i in range(len(s))]
    return t in rotated


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [line.rstrip().split(',') for line in f]

    for (s, t) in data:
        print if_string_rotation(s, t)
