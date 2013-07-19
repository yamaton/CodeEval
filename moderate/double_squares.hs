{-
double_squares.hs

Created by Yamato Matsuoka on 2012-07-04.

Description
------------
Credits: This challenge appeared in the Facebook Hacker Cup 2011.

A double-square number is an integer X which can be expressed as the sum of two perfect squares. For example, 10 is a double-square because 10 = 3^2 + 1^2. Your task in this problem is, given X, determine the number of ways in which it can be written as the sum of two squares. For example, 10 can only be written as 3^2 + 1^2 (we don't count 1^2 + 3^2 as being different). On the other hand, 25 can be written as 5^2 + 0^2 or as 4^2 + 3^2.

NOTE: Do NOT attempt a brute force approach. It will not work. The following constraints hold: 

0 <= X <= 2147483647
1 <= N <= 100

Input sample
-------------
You should first read an integer N, the number of test cases. The next N lines will contain N values of X.
```
5
10
25
3
0
1
```

Output sample
--------------
e.g.
```
1
2
0
1
1
```
-}

import System.Environment (getArgs)

def EratosthenesSieve(N):
    """Construct a list of primes equal or less than N."""
    numbers = [True] * (N+1)
    max_p = int(math.sqrt(N))
    for p in (i for i in range(2, max_p+1) if numbers[i]):
        for q in range(p*p, N+1, p):
            numbers[q] = False
    return [i for i in range(2, N+1) if numbers[i]]



def divisors(n):
    if n == 0: return None
    if n == 1: return [1]
    fi = factorinteger(n)
    itr = ([x**i for i in range(p+1)] for (x, p) in fi)
    return sorted(reduce(operator.mul, x) for x in itertools.product(*itr))


def is_square(n):
    x = int(n**0.5)
    return x*x == n


def count_square_sums(n):
    """Use Jacobi's two square theorem"""
    if n == 0: return 1
    total = 4*( sum(1 for i in divisors(n) if i % 4 == 1) 
              - sum(1 for i in divisors(n) if i % 4 == 3) )
    ## Remove duplicate countings if n > 0
    ##      Eight duplicates: (+/-a, +/-b) (+/-b, +/-a) 
    ##      Four duplicates: (0,+1), (0,-1), (+1,0), (-1,0)
    ##      Four duplicates: (+/-1,+/-1)
    flg = 0
    if is_square(n): flg += 1
    if is_square(n/2) and (n % 2 == 0): flg += 1
    return (total + 4*flg)/8    


def test():
    assert EratosthenesSieve(0) == []
    assert EratosthenesSieve(1) == []
    assert EratosthenesSieve(2) == [2]
    assert EratosthenesSieve(15) == [2, 3, 5, 7, 11, 13]
    
    assert factorinteger(0) == [(0, 1)]
    assert factorinteger(1) == [(1, 1)]
    assert factorinteger(12) == [(2, 2), (3, 1)]
    assert factorinteger(128) == [(2, 7)]
    
    assert divisors(0) == None
        
    assert divisors(1) == [1]
    assert divisors(5) == [1, 5]
    assert divisors(12) == [1, 2, 3, 4, 6, 12]
    
    assert is_square(0) is True
    assert is_square(1) is True
    assert is_square(2) is False
    assert is_square(101) is False
    
    assert count_square_sums(0) == 1
    assert count_square_sums(1) == 1
    assert count_square_sums(10) == 1
    assert count_square_sums(3) == 0
    assert count_square_sums(25) == 2
    print "passed all tests!"


countSquareSums :: Int -> Int
countSquareSums = undefined

main = do 
    f:_ <- getArgs
    contents <- readFile f
    let inputs  = map read $ lines contents
    let outputs = map countSquareSums inputs
    mapM print outputs
