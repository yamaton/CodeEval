#!/usr/bin/env python
# encoding: utf-8
"""
Word Search
===========
Description
-----------
Given a 2D board and a word, find if the word exists in the grid. The word can be constructed from letters of sequentially adjacent cell, where adjacent cells are those horizontally or vertically neighboring. The same letter cell may not be used more than once.

Input sample
------------
The board to be used may be hard coded as:

[
[ABCE],
[SFCS],
[ADEE]
]

Your program should accept as its first argument a path to a filename. Each line in this file contains a word. e.g.
```
ASADB
ABCCED
```

Output sample
-------------
Print out True if the word exists in the board, False otherwise. e.g.
```
False
True
```

"""
import sys


GRID = ['ABCE', 'SFCS', 'ADEE']


def word_search(s):
    row, col = len(GRID), len(GRID[0])

    string = "".join(GRID)
    pos = [i for (i, ch) in enumerate(string) if ch == s[0]]
    starts = [(i / row, i % col) for i in pos]
    for start in starts:
        current = start
        visited = [current]
    pass


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [s.rstrip() for s in f]

    for s in data:
        print word_search(s)