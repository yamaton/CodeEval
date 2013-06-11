#!/usr/bin/env python
# encoding: utf-8
"""
closest_pair.py

Created by Yamato Matsuoka on 2012-07-19.

Description:

Credits: Programming Challenges by Steven S. Skiena and Miguel A. Revilla

You will be given the x/y co-ordinates of several locations. You will be laying out 1 cable between two of these locations. In order to minimise the cost, your task is to find the shortest distance between a pair of locations, so that pair can be chosen for the cable installation.

Input sample:

Your program should accept as its first argument a path to a filename.The input file contains several sets of input. Each set of input starts with an integer N (0<=N<=10000), which denotes the number of points in this set. The next N line contains the coordinates of N two-dimensional points. The first of the two numbers denotes the X-coordinate and the latter denotes the Y-coordinate. The input is terminated by a set whose N=0. This set should not be processed. The value of the coordinates will be less than 40000 and non-negative. eg.

5
0 2
6 67
43 71
39 107
189 140
0

Output sample:

For each set of input produce a single line of output containing a floating point number (with four digits after the decimal point) which denotes the distance between the closest two points. If there is no such two points in the input whose distance is less than 10000, print the line INFINITY. eg.

36.2215



[Comment]

This is closest pair of points problem
http://en.wikipedia.org/wiki/Closest_pair_of_points_problem
"""

import sys
import operator
import math
import itertools


def closestdist2(points):
    N = len(points)    
    
    if N == 2:
        p1, p2 = points
        return (p1[0]-p2[0])**2 + (p1[1]-p2[1])**2
    elif N == 3:
        p1, p2, p3 = points
        d12 = (p1[0]-p2[0])**2 + (p1[1]-p2[1])**2
        d23 = (p2[0]-p3[0])**2 + (p2[1]-p3[1])**2
        d31 = (p3[0]-p1[0])**2 + (p3[1]-p1[1])**2
        return min(d12, d23, d31)
    
    points.sort()  ## Sort by x coordinate
    threshold = N//2
    pointsL = points[ :threshold]
    pointsR = points[threshold: ]
    
    xL, _ = pointsL[-1]
    xR, _ = pointsR[0]
    xmid = (xL + xR) / 2
    
    dmin = min(closestdist2(pointsL), closestdist2(pointsR))
    selectedL = ( (xL,yL) for (xL,yL) in pointsL if (xmid - xL) < dmin )
    selectedR = ( (xR,yR) for (xR,yR) in pointsR if xR < xmid + dmin )
    for (xL, yL) in selectedL:
        try:
            dLRmin = min( (xR-xL)**2 + (yR-yL)**2 for (xR,yR) in pointsR \
                        if abs(yR-yL) < dmin )
            dmin = min(dLRmin, dmin)
        except ValueError:
            pass
    return dmin


def format(dist_squared):
    if dist_squared > 10000:
        return "INFINITY"
    else:
        return "%.4f" % dist_squared


def readdata(f):
    f = [s.rstrip() for s in f if s.rstrip()]
    out = []
    bag = []
    for s in f:
        try:
            x = int(s)
            if bag:
                out.append(bag)
            bag = []
        except ValueError:
            x, y = s.split()
            bag.append((int(x), int(y)))
    return out


if __name__ == '__main__':
    sqrt = math.sqrt
    with open(sys.argv[1], "r") as f:
        points_set = readdata(f)
        min_dists = [sqrt(closestdist2(points)) for points in points_set]
        print "\n".join(format(d) for d in min_dists)

