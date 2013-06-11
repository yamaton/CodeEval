"""
Sudoku 

Description:

Sudoku is a number-based logic puzzle. It typically comprises of a 9*9 grid with digits so that each column, each row and each of the nine 3*3 sub-grids that compose the grid contains all the digits from 1 to 9. For this challenge, you will be given an N*N grid populated with numbers from 1 through N and you have to determine if it is a valid sudoku solution. You may assume that N will be either 4 or 9. The grid can be divided into square regions of equal size, where the size of a region is equal to the square root of a side of the entire grid. Thus for a 9*9 grid there would be 9 regions of size 3*3 each.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains the value of N, a semicolon and the sqaure matrix of integers in row major form, comma delimited. e.g.

4;1,4,2,3,2,3,1,4,4,2,3,1,3,1,4,2
4;2,1,3,2,3,2,1,4,1,4,2,3,2,3,4,1

Output sample:

Print out True/False if the grid is a valid sudoku layout. e.g.

True
False
"""
import sys
import math


def check_sudoku(state, n):
    SET = set(range(1,n+1))
    out = []
    for i in range(n):
        start = n * i
        end = n * (i + 1)
        out.append(set(state[start:end]) == SET)
        out.append(set(state[i::n]) == SET)

        # block
        blocksize = int(math.sqrt(n))
        row = i / blocksize
        col = i % blocksize
        tmp = [state[n * (blocksize * row + j) + (blocksize * col + k)] for j in range(blocksize) for k in range(blocksize)]
        out.append(set(tmp) == SET)

    return all(out)


def parser(s):
    head, tail = s.rstrip().split(';')
    size = int(head)
    entry = [int(x) for x in tail.split(',')]
    return (size, entry)


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [parser(s) for s in f]

    for (n, entry) in data:
        print check_sudoku(entry, n)
