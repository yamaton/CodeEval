#!/usr/bin/env python
# encoding: utf-8
"""
cycle_detection.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

Given a sequence, write a program to detect cycles within it.

Input sample:

A file containing a sequence of numbers (space delimited). The file can have multiple such lines. e.g

2 0 6 3 1 6 3 1 6 3 1

Ensure to account for numbers that have more than one digit eg. 12. If there is no sequence, ignore that line.

Output sample:

Print to stdout the first sequence you find in each line. Ensure that there are no trailing empty spaces on each line you print. e.g.

6 3 1

"""

import sys

# 
# def cycle_detection(seq):
#     """
#     Detect cycle in seq using Floyd's algorithm.
#     http://en.wikipedia.org/wiki/Cycle_detection
#     
#     This algorithm works given that sequence is as long as you want.
#     """
#     totoise = 1  # index for totoise
#     hare = 2     # index for hare
#     
#     while seq[totoise] != seq[hare]:
#         totoise += 1
#         hare += 2
#     
#     # Find the position of the first repetition of length mu
#     mu = 0
#     totoise = 0
#     while seq[totoise] != seq[hare]:
#         totoise += 1
#         hare += 1
#         mu += 1
#     
#     # Find the length lam of the shortest cycle starting from seq[mu]
#     lam = 1
#     hare = totoise + 1
#     while seq[totoise] != seq[hare]:
#         hare += 1
#         lam += 1
#     
#     return seq[mu:mu+lam]


def detect_cycle(seq):
    """
    Crude algorithm to find a cycle such that
    
    - Select the subsequence seq[n:] such that n is the minimum.
    - Return 1 cycle orbit of the subsequence.
    """
    for n in range(len(seq)):
        x = seq[n:]
        period = periodicity_len(x)
        if period > 0:
            return x[:period]
    else:
        return False


def periodicity_len(seq):
    """
    Return length of periodic orbit. seq must NOT contain transient.
    0 is returned if seq is aperiodic.
    """
    N = len(seq)
    head = seq[0]
    
    for i in range(1, N/2):
        try:
            periodicity = i + seq[i:].index(head) 
        except ValueError:
            continue
        if periodicity <= N/2:
            if all( seq[k+periodicity] == seq[k] for k in range(N-periodicity) ):
                return periodicity
    else:
        return 0




if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[int(x) for x in line.rstrip().split()] for line in f if line.rstrip()]
    
    out = (detect_cycle(x) for x in data if detect_cycle(x))
    print "\n".join(" ".join(str(i) for i in x) for x in out)
