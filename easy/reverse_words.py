#!/usr/bin/env python
# encoding: utf-8
"""
untitled.py

Created by Yamato Matsuoka on 2012-07-16.

Description:
------------

Write a program to reverse the words of an input sentence.


Input sample:
-------------

The first argument will be a text file containing multiple sentences, one per line. Possibly empty lines too. e.g. 

Hello World
Hello CodeEval


Output sample:
--------------

Print to stdout, each line with its words reversed, one per line. Empty lines in the input should be ignored. Ensure that there are no trailing empty spaces on each line you print. 
e.g.

World Hello
CodeEval Hello


"""

import sys

if __name__ == '__main__':
	with open(sys.argv[1], "r") as f:
	    data = [line.split() for line in f]
	    data = [" ".join(reversed(line)) for line in data]
	    print "\n".join(data)


