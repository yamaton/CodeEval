"""
prime_palindrome.py

Created by Yamato Matsuoka on 2012-07-16.

Description
-----------

Write a program to determine the biggest prime palindrome under 1000.


Input: None
Output: Prints the largest palindrome on stdout under 1000.

"""

import math


def integerdigits(n):
    """Construct list of decimal digits from the integer n."""
    x = []
    quotient = n
    while quotient > 0:
        x.append(quotient % 10)
        quotient /= 10
    x.reverse()
    return x
    

def fromdigits(x):
    """Constructs an integer from the list x of decimal digits."""
    return reduce(lambda i,j: 10*i + j, x)


def is_palindrome(n):
    """Return True if integer n is palindrome."""
    x = integerdigits(n)
    return n == fromdigits(reversed(x))

    
def is_prime(n):
    """Return True if integer n is prime number."""
    if n == 2:
        return True
    elif n % 2 == 0:
        return False
    else:
        max_p = int(math.sqrt(n))
        return all(n % p != 0 for p in range(3, max_p+1, 2))


def find_prime_palindrome(upperbound):
    for n in reversed(range(upperbound)):
        if is_palindrome(n) and is_prime(n):
            return n


upperbound = 1000
print find_prime_palindrome(upperbound)


