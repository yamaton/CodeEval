#!/usr/bin/env python
# encoding: utf-8
"""
lowercase.py

Created by Yamato Matsuoka on 2012-07-16.


Description:

Given a string write a program to convert it into lowercase.

Input sample:

The first argument will be a text file containing sentences, one per line. You can assume all characters are from the english language. e.g. 

HELLO CODEEVAL
This is some text

Output sample:

Print to stdout, the lowercase version of the sentence, each on a new line.
e.g.

hello codeeval
this is some text
"""

import sys


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [line.lower() for line in f.readlines()]
        print "".join(data)

