#!/usr/bin/env python
# encoding: utf-8
"""
filesize.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Print the size of a file in bytes.

Input sample:

Path to a filename. e.g. 

foo.txt

Output sample:

Print the size of the file in bytes.
e.g.

55
"""
import sys
import os

if __name__ == '__main__':
    print os.path.getsize(sys.argv[1])

