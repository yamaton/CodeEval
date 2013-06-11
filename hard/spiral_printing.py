#!/usr/bin/env python
# encoding: utf-8
"""
spiral_printing.py

Created by Yamato Matsuoka on 2012-07-19.

Input sample:

Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains three items (semicolon delimited). The first is 'n'(rows), the second is 'm'(columns) and the third is a single space separated list of characters/numbers in row major order. eg.

3;3;1 2 3 4 5 6 7 8 9

Output sample:

Print out the matrix in clockwise fashion, one per line, space delimited. eg.

1 2 3 6 9 8 7 4 5

"""

import sys


def reshape(seq, rows, cols):
    """
    This is numpy equivalent of reshape.
    row * col == len(seq) must be required.
    """
    if len(seq) != rows * cols:
        raise ValueError("row and column size are invalid.")
    return [seq[i*cols:(i+1)*cols] for i in range(rows)]
    

def spiral(seq, rows, cols):
    matrix = reshape(seq, rows, cols)
    delta = [[0, 1], [1, 0], [0, -1], [-1, 0]]
    maxX, maxY = rows, cols
    numbers = maxX * maxY
    
    x, y = 0, 0
    idx = 0
    result = [matrix[x][y]]
    while len(result) < numbers:
        next_x = x + delta[idx][0]
        next_y = y + delta[idx][1]
        if (0 <= next_x < maxX and 0 <= next_y < maxY 
                and matrix[next_x][next_y] not in result):
            x, y = next_x, next_y
            result.append(matrix[x][y])
        else:
            idx = (idx + 1) % len(delta)
            x += delta[idx][0]
            y += delta[idx][1]
            result.append(matrix[x][y])
    return result
        

def readdata(text):
    data = (line.split(";") for line in text.split("\n") if line)
    data = [(seq.split(), int(rows), int(cols)) for (rows, cols, seq) in data]
    return data



def test():
    seq = range(1, 7)
    assert reshape(seq, 3, 2) == [[1, 2], [3, 4], [5, 6]]
    assert reshape(seq, 2, 3) == [[1, 2, 3], [4, 5, 6]]
    assert reshape("a b c d e f".split(), 2, 3) == [['a', 'b', 'c'], ['d', 'e', 'f']]    
    
    seq = 'abcdef'
    assert spiral(seq, 3, 2) == ['a', 'b', 'd', 'f', 'e', 'c']
    seq = range(1,10)
    assert spiral(seq, 3, 3) == [1, 2, 3, 6, 9, 8, 7, 4, 5]
    seq = range(1,13)
    assert spiral(seq, 3, 4) == [1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7]
    assert spiral(seq, 4, 3) == [1, 2, 3, 6, 9, 12, 11, 10, 7, 4, 5, 8]

    text = "3;3;1 2 3 4 5 6 7 8 9\n4;3;1 2 3 4 5 6 7 8 9 10 11 12\n"
    assert readdata(text) == [([str(c) for c in range(1,10)], 3, 3), 
                              ([str(c) for c in range(1,13)], 4, 3)]
    print "all test passed"



if __name__ == '__main__':
    # test()
    with open(sys.argv[1], "r") as f:
        data = readdata(f.read())
    
    result = [spiral(seq, rows, cols) for (seq, rows, cols) in data]
    for row in result:
        print " ".join(row)

