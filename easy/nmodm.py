#!/usr/bin/env python
# encoding: utf-8
"""
N Mod M

Created by Yamato on 2013-02-05

Description:

Given two integers N and M, calculate N Mod M (without using any inbuilt modulus operator).

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated positive integers. e.g.

20,6
2,3
You may assume M will never be zero.

Output sample:

Print out the value of N Mod M
"""
import sys

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split(",")] for line in f]

    for (n, m) in data:
        print n % m