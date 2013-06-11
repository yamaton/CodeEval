"""
fizzbuzz.py

Created by Yamato Matsuoka on 2012-07-16.



How to Use
----------

>> python fizzbuzz.py inputfile.txt



About the Program
-----------------

Input file must contain lines of three numbers p, q, and N.

Output is sequence f(i) for 1 <= i <= N separated by single space.

f(i) = "FB"    if (i % p) == 0 and (i % q) == 0 
     = "F"     elif (i % p) == 0
     = "B"     elif (i % q) == 0
     = i       otherwise
"""

import sys

def fizzbuzz(i, p, q):
    """
    fizzbuzz string
    """
    if (i % p == 0) and (i % q == 0):
        return "FB"
    elif i % p == 0:
        return "F"
    elif i % q == 0:
        return "B"
    else:
        return str(i)

def transform(lis):
    (p, q, N) = lis
    return (fizzbuzz(i, p, q) for i in range(1,N+1))



filename = sys.argv[1]

with open(filename, "r") as f:
    data = ([int(i) for i in line.split()] for line in f)
    
    for entry in data:
        s = " ".join(transform(entry))
        print s

    