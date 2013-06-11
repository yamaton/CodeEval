#!/usr/bin/env python
# encoding: utf-8
"""
longestlines.py

Created by Yamato Matsuoka on 2012-07-16.


Description:

Write a program to read a multiple line text file and write the 'N' longest lines to stdout. Where the file to be read is specified on the command line.


Input sample:

Your program should read an input file (the first argument to your program). The first line contains the value of the number 'N' followed by multiple lines. You may assume that the input file is formatted correctly and the number on the first line i.e. 'N' is a valid positive integer.e.g.

2
Hello World

CodeEval
Quick Fox
A
San Francisco


Output sample:

The 'N' longest lines, newline delimited. Do NOT print out empty lines. Ignore all empty lines in the input. Ensure that there are no trailing empty spaces on each line you print. Also ensure that the lines are printed out in decreasing order of length i.e. the output should be sorted based on their length e.g.

San Francisco
Hello World

"""

import sys
import os


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        tmp = f.readlines()
        N = int(tmp[0])
        lis = (s.rstrip() for s in tmp[1:])
        lis = [s for s in lis if s]
        lis.sort(key=len, reverse=True)
    
    print "\n".join(lis[:N])