#!/usr/bin/env python
# encoding: utf-8
"""
self_describing_numbers.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

A number is a self-describing number when (assuming digit positions are labeled 0 to N-1), the digit in each position is equal to the number of times that that digit appears in the number.

Input sample:

The first argument is the pathname to a file which contains test data, one test case per line. Each line contains a positive integer. Each line is in the format: N i.e. a positive integer eg.

2020
22
1210

Output sample:

If the number is a self-describing number, print out a 1. If not, print out a 0 eg.

1
0
1

For the curious, here's how 2020 is a self-describing number: Position '0' has value 2 and there is two 0 in the number. Position '1' has value 0 because there are not 1's in the number. Position '2' has value 2 and there is two 2. And the position '3' has value 0 and there are zero 3's.
"""

import sys

def integerdigits(n):
    """
    Construct list of decimal digits from the integer n.
    """
    x = []
    quotient = n
    while quotient > 0:
        x.append(quotient % 10)
        quotient /= 10
    x.reverse()
    return x



def is_self_describing_number(n):
    """Return 1 (otherwise 0) if integer n is a self-describing number"""
    x = integerdigits(n)
    predicate = all(x[pos] == x.count(pos) for pos in range(len(x)))
    if predicate:
        return 1
    else:
        return 0



if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(x) for x in f]
    
    out = (is_self_describing_number(n) for n in seq)
    print "\n".join(str(n) for n in out)
    
