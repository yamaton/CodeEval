#!/usr/bin/env python
# encoding: utf-8

"""
Reverse Groups

Description:

Given a list of numbers and a positive integer k, reverse the elements of the list, k items at a time. If the number of elements is not a multiple of k, then the remaining items in the end should be left as is.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains a list of numbers and the number k, separated by a semicolon. The list of numbers are comma delimited. e.g.

1,2,3,4,5;2
1,2,3,4,5;3

Output sample:

Print out the new comma separated list of numbers obtained after reversing. e.g.

2,1,4,3,5
3,2,1,4,5

"""
import sys


def reverse_group(group, k):
    if len(group) < k:
        ## just in case
        k = len(group)

    r = len(group) % k
    if r > 0:
        body, remainder = group[:-r], group[-r:]
    else:
        body, remainder = group, []

    for cnt in range(len(body) / k):
        begin = k * cnt
        end = k * (cnt + 1)
        body[begin:end] = list(reversed(body[begin:end]))

    return body + remainder


def parser(s):
    head, tail = s.rstrip().split(';')
    return ([int(i) for i in head.split(',')], int(tail))

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [parser(s) for s in f if s.rstrip()]

    for (group, k) in data:
        x = reverse_group(group, k)
        print ",".join(str(i) for i in x)

