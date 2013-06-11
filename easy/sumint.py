#!/usr/bin/env python
# encoding: utf-8
"""
sumint.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Print out the sum of integers read from a file.

Input sample:

The first argument to the program will be a text file containing a positive integer, one per line. e.g. 

5
12

NOTE: For solutions in JavaScript, assume that there are 7 lines of input
Output sample:

Print out the sum of all the integers read from the file. 
e.g.

17
"""

import sys


def main():
	pass


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        print sum(int(i) for i in f)
    

