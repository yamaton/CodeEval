#!/usr/bin/env python
# encoding: utf-8
"""
tables.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Print out the grade school multiplication table upto 12*12.

Input sample:

None

Output sample:

Print out the table in a matrix like fashion, each number formatted to a width of 4 (The numbers are right-aligned and strip out leadeing/trailing spaces on each line). The first 3 line will look like: 
e.g.

1   2   3   4   5   6   7   8   9  10  11  12
2   4   6   8  10  12  14  16  18  20  22  24
3   6   9  12  15  18  21  24  27  30  33  36
"""

if __name__ == '__main__':

    table = [[i*j for i in range(1, 13)] for j in range(1,13)]
    out = ["".join("%4d"%x for x in row) for row in table]
    print "\n".join( s[2:] for s in out )

