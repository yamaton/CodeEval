"""
Distinct Subsequences
=======================
Challenge Description
----------------------
A subsequence of a given sequence S consists of S with zero or more elements deleted. Formally, a sequence Z = z1z2..zk is a subsequence of X = x1x2...xm, if there exists a strictly increasing sequence <i1,i2...ik> of indicies of X such that for all j=1,2,...k we have Xij = Zj. e.g. Z=bcdb is a subsequence of X=abcbdab with corresponding index sequence <2,3,5,7>

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated strings. The first is the sequence X and the second is the subsequence Z. e.g.
```
babgbag,bag
rabbbit,rabbit
```

Output sample
--------------
Print out the number of distinct occurrences of Z in X as a subsequence e.g.
```
5
3
```
"""
import sys
import itertools


def distinct_subsequences(x, z):
    subsequences = ["".join(seq) for seq in itertools.combinations(list(x), len(z))]
    return subsequences.count(z)

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [line.rstrip().split(',') for line in f]
    out = [distinct_subsequences(x, z) for [x, z] in data]
    for i in out:
        print i
