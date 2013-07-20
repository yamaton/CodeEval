"""
prefix.py

Created by Yamato Matsuoka on 2012-07-17.

Description:

You are given a prefix expression. Write a program to evaluate it.

Input sample:

The first argument will be an input file with one prefix expression per line. e.g.

* + 2 3 4

Your program has to read this and insert it into any data structure you like. Traverse that data structure and evaluate the prefix expression. Each token is delimited by a whitespace. You may assume that the only valid operators appearing in test data are '+','*' and '/' 

Output sample:

Print to stdout, the output of the prefix expression, one per line. e.g.

20

"""

import sys
import operator
        
def interpret(ch):
    """Interprete char ch.
    If ch is digit char, return integer.
    If ch is either +, *, /, return operator function.
    """
    d = {"+": operator.add, "*": operator.mul,
         "-": operator.sub, "/": operator.div}
    return d[ch] if ch in d else int(ch)


def evaluate_prefix_expression(seq):
    """evaluate prefix expression using stack method"""
    stack = []
    for x in reversed(seq):
        if isinstance(x, int):
            stack.append(x)
        else:
            x1 = stack.pop()
            x2 = stack.pop()
            out = x(x1, x2)
            stack.append(out)
    return stack[0]

with open(sys.argv[1], "r") as f:
    data = [[interpret(x) for x in line.split()] for line in f]
out = (evaluate_prefix_expression(x) for x in data)
print "\n".join(str(n) for n in out)
