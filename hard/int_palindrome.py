"""
Description:

A positive integer is a palindrome if its decimal representation (without leading zeros) is a palindromic string (a string that reads the same forwards and backwards). For example, the numbers 5, 77, 363, 4884, 11111, 12121 and 349943 are palindromes.

A range of integers is interesting if it contains an even number of palindromes. The range [L, R], with L <= R, is defined as the sequence of integers from L to R (inclusive): (L, L+1, L+2, ..., R-1, R). L and R are the range's first and last numbers.

The range [L1,R1] is a subrange of [L,R] if L <= L1 <= R1 <= R. Your job is to determine how many interesting subranges of [L,R] there are.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain two positive integers, L and R (in that order), separated by a space. eg.

1 2
1 7
87 88

Output sample:

For each line of input, print out the number of interesting subranges of [L,R] eg.

1
12
1

For the curious: In the third example, the subranges are: [87](0 palindromes), [87,88](1 palindrome),[88](1 palindrome). Hence the number of interesting palindromic ranges is 1
"""

import sys

def is_palindrome(number):
    s = str(number)
    return s == "".join(reversed(s))

def subranges(seq):
    X = len(seq)
    for i in range(X):
        for j in range(i+1, X+1):
            yield seq[i:j]

def getdata(filename):
    with open(filename, "r") as f:
         return [map(int, row.split()) for row in f if row.rstrip()]

def interesting_subseq_number(start, end):
    tflist = [is_palindrome(i) for i in xrange(start, end+1)]
    return len([sub for sub in subranges(tflist) if sum(sub) % 2 == 0])

def test():
    data = getdata('./int_palindrome.txt')
    assert data == [[1, 2], [1, 7], [87, 88]]
    assert is_palindrome(5) == True
    assert is_palindrome(121) == True
    assert is_palindrome(122) == False
    
    assert [i for i in subranges(range(3))] == [[0], [0,1], [0, 1, 2], [1], [1, 2], [2]]
    
    assert interesting_subseq_number(1,1) == 0
    assert interesting_subseq_number(1,2) == 1
    assert interesting_subseq_number(1,7) == 12
    assert interesting_subseq_number(87,88) == 1
    
    print "passed all tests!"



if __name__ == '__main__':
    data = getdata(sys.argv[1])
    result = (interesting_subseq_number(start, end) for (start, end) in data)
    for i in result:
        print i

