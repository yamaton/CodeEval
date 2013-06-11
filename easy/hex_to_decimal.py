#!/usr/bin/env python
# encoding: utf-8
"""
Hex to Decimal
Created by Yamato Matsuoka on 2013-02-05

Description:

You will be given a hexadecimal(base 16) number. Convert it into decimal (base 10)

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains a hex number. You may assume that the hex number does not have the leading 'Ox'. Also all alpha characters(e.g. a through f) in the input will be in lowercase e.g.

9f
11

Output sample:

Print out the equivalent decimal number e.g.

159
17

"""
import sys

def hex2dec(s):
    return int(s, 16)

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        seq = [x for x in f]
    for i in seq:
        print hex2dec(i)