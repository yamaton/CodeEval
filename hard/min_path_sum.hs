{-
Minimum Path Sum 
=================
Description
-----------
You are given an n*n matrix of integers. You can move only right and down. Calculate the minimal path sum from the top left to the bottom right

Input sample
-------------
Your program should accept as its first argument a path to a filename. The first line will have the value of n(the size of the square matrix). This will be followed by n rows of the matrix. (Integers in these rows will be comma delimited). After the n rows, the pattern repeats. e.g.
```
2
4,6
2,8
3
1,2,3
4,5,6
7,8,9
```

Output sample
---------------
Print out the minimum path sum for each matrix. e.g.
```
14
21
```
-}

import System.Environment (getArgs)


def min_path_sum(N, data):
    return _rsum(data, N-1, N-1)


def _rsum(data, i, j):
    N = len(data)
    if i < 0 or j < 0 or i >= N or j >= N:
        return 100000
    elif(i == 0 and j == 0):
        return data[0][0]
    else:
        return min(_rsum(data, i-1, j), _rsum(data, i, j-1)) + data[i][j]



def read_data(text):
    out = []
    lines = [l.rstrip() for l in text]
    ptr = 0
    while ptr < len(lines):
        size = int(lines[ptr])
        start = ptr + 1
        data = [[int(n) for n in l.split(',')] for l in lines[start:start+size]]
        out.append((size, data))
        ptr += size + 1
    return out


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = read_data(f)
    for (n, matrix) in data:
        print min_path_sum(n, matrix)
