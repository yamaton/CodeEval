{-
lcs.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
------------
You are given two sequences. Write a program to determine the longest common subsequence between the two strings(each string can have a maximum length of 50 characters). (NOTE: This subsequence need not be contiguous. The input file may contain empty lines, these need to be ignored.)

Input sample
-------------
The first argument will be a file that contains two strings per line, semicolon delimited. You can assume that there is only one unique subsequence per test case. e.g.
```
XMJYAUZ;MZJAWXU
```

Output sample
-------------
The longest common subsequence. Ensure that there are no trailing empty spaces on each line you print. e.g.
```
MJAU
```
-}

import System.Environment

def lcs_length_matrix(s1, s2):
    M, K = len(s1), len(s2)
    C = [[0 for j in range(K+1)] for i in range(M+1)]
    for i in range(M):
        for j in range(K):
            C[i+1][j+1] = C[i][j] + 1 if s1[i] == s2[j] else max(C[i+1][j], C[i][j+1])
    return C        

def backtrack(C, s1, s2, i, j):
    """Find the longest common subsequence between two strings s1 and s2"""
    if i==-1 or j==-1:
        return ""
    elif s1[i] == s2[j]:
        return backtrack(C, s1, s2, i-1, j-1) + s1[i]
    else:
        if C[i+1][j] > C[i][j+1]:
            return backtrack(C, s1, s2, i, j-1)
        else:
            return backtrack(C, s1, s2, i-1, j)
    
def lcs(s1, s2):
    C = lcs_length_matrix(s1, s2)
    return backtrack(C, s1, s2, len(s1)-1, len(s2)-1)

def test():
    assert lcs("abc", "abc") == "abc"
    assert lcs("XMJYAUZ", "MZJAWXU") == "MJAU"
    print("passed all tests!")


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [s.rstrip().split(';') for s in f if s.rstrip()]
    results = (lcs(s1, s2) for (s1, s2) in data)
    for s in results:
         print s
    
    
    