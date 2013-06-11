"""
Minimum Coins  Share on LinkedIn

Description:

You are given 3 coins of value 1, 3 and 5. You are also given a total which you have to arrive at. Find the minimum number of coins to arrive at it.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains a positive integer number which represents the total you have to arrive at e.g.

11
20

Output sample:

Print out the minimum number of coins required to arrive at the total e.g.

3
4
"""
import sys

def min_coins(n):
    return (n / 5) + ((n % 5) / 3) + ((n % 5) % 3)

if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [int(x) for x in f]

    for n in data:
        print min_coins(n)
