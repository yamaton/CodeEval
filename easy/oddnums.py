#!/usr/bin/env python
# encoding: utf-8
"""
oddnums.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Print the odd numbers from 1 to 99.

Input sample:

None

Output sample:

Print the odd numbers from 1 to 99, one number per line.
"""

import sys

def is_odd(n):
    return n % 2 > 0 

if __name__ == '__main__':
	oddnums = (i for i in range(1,100) if is_odd(i))
	sys.stdout.write("\n".join(str(n) for n in oddnums))

