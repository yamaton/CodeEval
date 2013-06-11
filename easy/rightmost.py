#!/usr/bin/env python
# encoding: utf-8
"""
rightmost.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

You are given a string 'S' and a character 't'. Print out the position of the rightmost occurrence of 't'(case matters) in 'S' or -1 if there is none. The position to be printed out is zero based.

Input sample:

The first argument is a file, containing a string and a character, comma delimited, one per line. Ignore all empty lines in the input file.e.g. 

Hello World,r
Hello CodeEval,E

Output sample:

Print out the zero based position of the character 't' in string 'S', one per line. Do NOT print out empty lines between your output.
e.g.

8
10
"""

import sys

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [s.rstrip().split(",") for s in f]
    
    out = (s.rfind(t) for (s,t) in data)
    print "\n".join(str(i) for i in out)
    
