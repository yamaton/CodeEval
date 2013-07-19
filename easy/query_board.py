#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Query Board
============

Description
-----------

There is a board (matrix). Every cell of the board contains one integer, which is 0 initially.

The next operations can be applied to the Query Board:
SetRow i x: it means that all values in the cells on row "i" have been changed to value "x" after this operation.
SetCol j x: it means that all values in the cells on column "j" have been changed to value "x" after this operation.
QueryRow i: it means that you should output the sum of values on row "i".
QueryCol j: it means that you should output the sum of values on column "j".

The board's dimensions are 256x256
"i" and "j" are integers from 0 to 255
"x" is an integer from 0 to 31


Input sample
-------------

Your program should accept as its first argument a path to a filename. Each line in this file contains an operation of a query. E.g.

SetCol 32 20
SetRow 15 7
SetRow 16 31
QueryCol 32
SetCol 2 14
QueryRow 10
SetCol 14 0
QueryRow 15
SetRow 10 1
QueryCol 2


Output sample
--------------
For each query, output the answer of the query. E.g.

5118
34
1792
3571

"""
import sys


class Board:
    def __init__(self, size=256):
        self.size = size
        self.mat = [[0 for _ in range(size)] for __ in range(size)]

    def __str__(self):
        s = ''
        for row in range(self.size):
            tmp = "\t".join(str(self.mat[row][col]) for col in range(self.size))
            tmp += "\n"
            s += tmp
        return s

    def setcol(self, col, x):
        for row in range(self.size):
            self.mat[row][col] = x

    def setrow(self, row, x):
        for col in range(self.size):
            self.mat[row][col] = x

    def colsum(self, col):
        print sum(self.mat[row][col] for row in range(self.size))

    def rowsum(self, row):
        print sum(self.mat[row][col] for col in range(self.size))


def play(queries):
    m = Board()
    interpreter = {'SetCol': m.setcol, 'SetRow': m.setrow,
                   'QueryCol': m.colsum, 'QueryRow': m.rowsum}

    for (command, args) in queries:
        interpreter[command](*args)


def readline(s):
    query = s.rstrip().split()
    return (query[0], [int(x) for x in query[1:]])


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        commands = [readline(s) for s in f]

    play(commands)
