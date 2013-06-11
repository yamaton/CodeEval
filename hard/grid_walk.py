#!/usr/bin/env python
# encoding: utf-8
"""
grid_walk.py

Created by Yamato Matsuoka on 2012-07-19.

Description:

There is a monkey which can walk around on a planar grid. The monkey can move one space at a time left, right, up or down. That is, from (x, y) the monkey can go to (x+1, y), (x-1, y), (x, y+1), and (x, y-1). Points where the sum of the digits of the absolute value of the x coordinate plus the sum of the digits of the absolute value of the y coordinate are lesser than or equal to 19 are accessible to the monkey. For example, the point (59, 79) is inaccessible because 5 + 9 + 7 + 9 = 30, which is greater than 19. Another example: the point (-5, -7) is accessible because abs(-5) + abs(-7) = 5 + 7 = 12, which is less than 19. How many points can the monkey access if it starts at (0, 0), including (0, 0) itself?

Input sample:

There is no input for this program.

Output sample:

Print the number of points the monkey can access. It should be printed as an integer — for example, if the number of points is 10, print "10", not "10.0" or "10.00", etc.

"""

def digits_sum(n):
    x = 0
    quotient = abs(n)
    while quotient > 0:
        x += quotient % 10
        quotient /= 10
    return x


def scan_grid():
    ini = (0, 0)
    queue = [ini]
    visited = set([ini])
    delta = [(0, 1), (1, 0)]

    while queue:
        p = queue.pop()
        for direction in delta:
            x = p[0] + direction[0]
            y = p[1] + direction[1]
            if is_accessible(x, y) and (x, y) not in visited:
                queue.append((x, y))
                visited.add((x, y))
    return visited


def count(points):
    edge_num = len([1 for (x, y) in points if x == y or y == 0]) - 1
    inner_num = len(points) - edge_num - 1
    return 8 * inner_num + 4 * edge_num + 1


def is_accessible(x, y):
    if x < 0 or y < 0:
        return False
    elif y > x:
        return False
    else:
        return digits_sum(x) + digits_sum(y) <= 19


def count_grid_walk():
    return count(scan_grid())


if __name__ == '__main__':
    print count_grid_walk()
