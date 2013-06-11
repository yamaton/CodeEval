#!/usr/bin/env python
# encoding: utf-8
"""
untitled.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Write a program to determine the lowest common ancestor of two nodes in a binary search tree. You may hardcode the following binary search tree in your program:

    30
    |
  ____
  |   |
  8   52
  |
____
|   |
3  20
    |
   ____
  |   |
  10 29
  
Input sample:

The first argument will be a text file containing 2 values that represent two nodes within the tree, one per line. e.g. 

8 52
3 29

Output sample:

Print to stdout, the least common ancestor, one per line.
e.g.

30
8

"""

import sys

d = {30:(8,52), 8:(3,20), 20:(10,29)}

def lookup(n, curr=[30], stack=[]):
    """return sequence of the tree from top to n"""
    isa = isinstance
    
    if n in curr:
        stack.append(n)
        return stack
    for i in curr:
        if d.has_key(i):
            stack.append(i)
            return lookup(n,d[i],stack)


def commonListLast(s1,s2):
    out = []
    length = min(len(s1), len(s2))
    for i in range(length):
        x = s1[i]
        if x == s2[i]:
            out.append(x)
        else:
            break
    return out[-1]


def lcm(n1,n2):
    s1 = lookup(n1,curr=[30],stack=[])
    s2 = lookup(n2,curr=[30],stack=[])
    return commonListLast(s1,s2)



if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.split()] for line in f if line.rstrip()]
    out = (lcm(n1,n2) for (n1,n2) in data)
    print "\n".join(str(i) for i in out)

