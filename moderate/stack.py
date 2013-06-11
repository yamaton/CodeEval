#!/usr/bin/env python
# encoding: utf-8
"""
stack.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Write a program implementing a stack inteface for integers. The interface should have 'push' and 'pop' functions. You will be asked to 'push' a series of integers and then 'pop' and print out every alternate integer.

Input sample:

The first argument will be a text file containing a series of space delimited integers, one per line. e.g. 

1 2 3 4
10 -2 3 4

Output sample:

Print to stdout, every alternate integer(space delimited), one per line.
e.g.

4 2
4 -2

"""

import sys

class stack(object):
    def __init__(self):
        self.x = []
    
    def push(self, n):
        self.x.append(n)
    
    def pop(self):
        return self.x.pop()
    
    def flush(self):
        self.x = []



def push_and_pop(entry):
    s = stack()
    out = []
    for i in entry:
        s.push(i)
    for j in range(len(entry)):
        tmp = s.pop()
        if j % 2 == 0:
            out.append(tmp)
    return out


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.split()] for line in f]
    
    out = (push_and_pop(x) for x in data)
    out = (" ".join(str(i) for i in x) for x in out)
    print "\n".join(out)
            


