"""
Description:

Credits: This challenge has appeared in a past google competition

You are writing out a list of numbers. Your list contains all numbers with exactly Di digits in its decimal representation which are equal to i, for each i between 1 and 9, inclusive. You are writing them out in ascending order. For example, you might be writing every number with two '1's and one '5'. Your list would begin 115, 151, 511, 1015, 1051. Given N, the last number you wrote, compute what the next number in the list will be. The number of 1s, 2s, ..., 9s is fixed but the number of 0s is arbitrary.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain an integer n < 10^6

Output sample:

For each line of input, generate a line of output which is the next integer in the list.
"""
import sys
import itertools


def integerdigits(n):
    """
    Construct list of decimal digits from the integer n
    """
    if isinstance(n, str): n = int(n)
    if n == 0: return [0]
    x = []
    quotient = n
    while quotient > 0:
        x.append(quotient % 10)
        quotient /= 10
    x.reverse()
    return tuple(x)

def fromdigits(iterable): 
    "Constructs an integer from the list x of decimal digits."
    return reduce(lambda i,j: 10 * i + j, iterable)


def next_lexicographic_permutation(iterable):
    """
    Next lexicographic permutation is generated in list. 
    """
    x = list(iterable)
    try:
        k = max(i for i in range(len(x)-1) if x[i] < x[i+1])
    except ValueError:
        return None
    
    l = max(j for j in range(k+1, len(x)) if x[k] < x[j])
    x[k], x[l] = x[l], x[k]
    upper = x[:k+1]
    lower = x[k+1:]
    lower.reverse()
    return upper + lower


def next_number(n):
    """
    Treat nonzero numbers and zero numbers separately.
    """
    digits = integerdigits(n)
    x = next_lexicographic_permutation(digits)
    if x:
        return fromdigits(x)
    else:
        zerocount = digits.count(0)
        nonzero_digits = [d for d in sorted(digits) if d > 0]
        head = nonzero_digits[0]
        rest = nonzero_digits[1:] 
        return fromdigits([head] + [0]*(zerocount+1) + rest)


def test():
    assert next_lexicographic_permutation("1081021") == list("1081102")
    assert next_lexicographic_permutation("211") == None
    assert next_number(105) == 150
    
    assert next_number(115) == 151
    assert next_number(511) == 1015
    assert next_number(1501) == 1510
    assert next_number(52110) == 100125
    print "passed all tests!"


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [int(i) for i in f]
    result = map(next_number, data)
    for n in result:
        print n
