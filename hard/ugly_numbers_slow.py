#!/usr/bin/env python
# encoding: utf-8
"""
ugly_numbers.py

Created by Yamato Matsuoka on 2012-07-18.

Description:

Credits: This challenge has appeared in a google competition before.

Once upon a time in a strange situation, people called a number ugly if it was divisible by any of the one-digit primes (2, 3, 5 or 7). Thus, 14 is ugly, but 13 is fine. 39 is ugly, but 121 is not. Note that 0 is ugly. Also note that negative numbers can also be ugly; -14 and -39 are examples of such numbers.

One day on your free time, you are gazing at a string of digits, something like:

123456

You are amused by how many possibilities there are if you are allowed to insert plus or minus signs between the digits. For example you can make:

1 + 234 - 5 + 6 = 236

which is ugly. Or

123 + 4 - 56 = 71

which is not ugly.

It is easy to count the number of different ways you can play with the digits: Between each two adjacent digits you may choose put a plus sign, a minus sign, or nothing. Therefore, if you start with D digits there are 3^(D-1) expressions you can make. Note that it is fine to have leading zeros for a number. If the string is '01023', then '01023', '0+1-02+3' and '01-023' are legal expressions.

Your task is simple: Among the 3^(D-1) expressions, count how many of them evaluate to an ugly number.


Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will be a single line containing a non-empty string of decimal digits. The string in each test case will be non-empty and will contain only characters '0' through '9'. Each string is no more than 13 characters long. eg.

1
9
011
12345

Output sample:

Print out the number of expressions that evaluate to an ugly number for each test case, each one on a new line eg

0
1
6
64

"""

import sys
import operator as op
import itertools
import collections

def fromdigits(x):
    """
    Constructs an integer from the list x of decimal digits.
    
    Input: list of decimal digits 
    Output: integer
    """
    return reduce(lambda i,j: 10*i + j, x)
    

def tuples(lis,n):
    """
    Generates a list of all possible n-tuples of elements from list lis
    """
    out = []
    for i in range(n):
        out.append(lis)
    return itertools.product(*out)


def is_ugly(n):
    """Number is ugly if it's divisible by any of one-digit primes 
    i.e. (2,3,5,7)
    """
    return any((n % i) == 0 for i in (2,3,5,7))


def concat_digits(a, b):
    """
    Concatenate digits of integers a b.
    
    [example]  
        concatenate(35, 2) ==> 352
        concatenate(-1, 1) ==> -11
    """
    if a >= 0:
        return 10*a + b
    else:
        return (-1)*((-10)*a + b)


def concat_combinations(seq):
    return [seq[i:i+k] for k in range(2,len(seq)) for i in range(len(seq)-k+1)]
        




def count_ugly_numbers(s):
    """
    use prefix notation to evaluate.
    """
    s = [int(i) for i in s]
    n = len(s)
    
    result = []
    for operators in tuples((concat_digits, op.add, op.sub), n-1):
        stack = s[:]

        ## Do concatinations first
        operators = list(operators)
        while concat_digits in operators:
            idx = operators.index(concat_digits)
            stack = stack[:idx] + \
                    [concat_digits(stack[idx], stack[idx+1])] +\
                    stack[idx+2:]
            operators.remove(concat_digits)
            n -= 1
        
        ## Then carry out plus and minus
        stack.reverse()
        for f in operators:
            x1 = stack.pop()
            x2 = stack.pop()
            out = f(x1, x2)
            stack.append(out)
        
        result.append(stack[0])
        
    # debug = sorted([i for i in result if is_ugly])
    # print debug
    return sum(is_ugly(i) for i in result)


if __name__ == '__main__':
    # with open(sys.argv[1], "r") as f:
    #     data = [s.rstrip() for s in f if s.rstrip()]
    # 
    # out = (count_ugly_numbers(s) for s in data)
    # print "\n".join(str(i) for i in out)

    # print count_ugly_numbers("02180737102")
    # print count_ugly_numbers("0218073710232")
    # print count_ugly_numbers("12345")
    # print is_ugly(-31)
    
    print concat_combinations(range(10))

            
