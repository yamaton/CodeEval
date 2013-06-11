#!/usr/bin/env python
# encoding: utf-8
"""
discout_offers.py

http://codeeval.com/open_challenges/48/

Discount Offers
===============

Description:
------------

Our marketing department has just negotiated a deal with several local merchants that will allow us to offer exclusive discounts on various products to our top customers every day. The catch is that we can only offer each product to one customer and we may only offer one product to each customer.

Each day we will get the list of products that are eligible for these special discounts. We then have to decide which products to offer to which of our customers. Fortunately, our team of highly skilled statisticians has developed an amazing mathematical model for determining how likely a given customer is to buy a given product by calculating what we call the "suitability score" (SS). The top-secret algorithm to calculate the SS between a customer and a product is this:

1. If the number of letters in the product's name is even then the SS is the number of vowels (a, e, i, o, u, y) in the customer's name multiplied by 1.5.
2. If the number of letters in the product's name is odd then the SS is the number of consonants in the customer's name.
3. If the number of letters in the product's name shares any common factors (besides 1) with the number of letters in the customer's name then the SS is multiplied by 1.5.

Your task is to implement a program that assigns each customer a product to be offered in a way that maximizes the combined total SS across all of the chosen offers. Note that there may be a different number of products and customers.

You may include code from external libraries as long as you cite the source.

Input sample:
-------------

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain a set of comma delimited customer names followed by a semicolon and then a set of comma delimited product names. eg.

Jack Abraham,John Evans,Ted Dziuba;iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow

Jeffery Lebowski,Walter Sobchak,Theodore Donald Kerabatsos,Peter Gibbons,Michael Bolton,Samir Nagheenanajar;Half & Half,Colt M1911A1,16lb bowling ball,Red Swingline Stapler,Printer paper,Vibe Magazine Subscriptions - 40 pack

Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage

Output sample:
--------------

For each line of input, print out the maximum total score to two decimal places eg.

21.00
83.50
71.25

[LOG]
Ver 0.13 Fixed: # of product may be greater than # of customers
Ver 0.12 Back to Ver 0.10 with effective memorization (SS table)
Ver 0.11 Table method.
Ver 0.10 Brute-force with memorization. Works for the sample inputs but too slow.

[Comments]
    This is a linear assignment problem.
    http://en.wikipedia.org/wiki/Assignment_problem
    Implement Hungarian algorithm.

"""
import itertools
import sys
import time
import functools


def decorator(d):
    """Make function d a decorator: d wraps a function fn.
    Based on Peter Norvig's code in Udacity. """
    update_wrapper = functools.update_wrapper

    def _d(fn):
        return update_wrapper(d(fn), fn)
    update_wrapper(_d, d)
    return _d


@decorator
def memo(f):
    """Decorator that caches the return value for each call to f(args).
    Then when called again with same args, we can just look it up."""
    cache = {}

    def _f(*args):
        try:
            return cache[args]
        except KeyError:
            cache[args] = result = f(*args)
            return result
        except TypeError:
            # some element of args can't be a dict key
            return f(args)
    return _f


def gcd(a, b):
    """Using Euclid's algorithm"""
    if b > a:
        a, b = b, a
    if b == 0:
        return a
    else:
        return gcd(b, a - b * (a // b))


@memo
def suitability_score(c_name, p_name):
    """ Compute suitability score from a customer's name (c_name)
    and a product's name (p_name).
    """
    num_letters_customer = len([i for i in c_name if i.isalpha()])
    num_vowels_customer = len([i for i in c_name.lower() if i in 'aeiouy'])
    num_letters_product = len([i for i in p_name if i.isalpha()])

    if num_letters_product % 2 == 0:
        ss = 1.5 * num_vowels_customer
    else:
        ss = num_letters_customer - num_vowels_customer

    if gcd(num_letters_customer, num_letters_product) > 1:
        ss *= 1.5
    return ss


def hungarian(c):
    """
    hungarian(list(list(int))) -> int

    This is implementation of Hungarian (Kuhn-Munkres) algorithm.
    c is 2-D list (N x N matrix) of integers (or float) called "cost matrix".
    Returns the minimum assignment from the cost matrix.
    """
    N = len(c)
    assert N == len(c[0])

    c = _hungarian_step0(c)

    while True:
        best_edges = _konig(c)
        if _is_solved(c):
            return best_edges
        else:
            c = _hungarian_step2(c)


def _hungarian_step0(costmatrix):
    """
    helper function of hungarian: Step 0
    """
    N = len(costmatrix)
    for i in xrange(N):
        rowmin = min(costmatrix[i][j] for j in xrange(N))
        for j in xrange(N):
            costmatrix[i][j] -= rowmin

    for j in xrange(N):
        colmin = min(costmatrix[i][j] for i in xrange(N))
        for i in xrange(N):
            costmatrix[i][j] -= colmin
    return costmatrix


def _konig(costmatrix):
    """
    Find the minimum vertex cover V according to König's theorem.
    """
    pass


def _is_solved(costmatrix):
    """
    _is_solved(matrix) --> boolean

    Check if the assignment problem is solved.
    """
    pass


def _hungarian_step2(costmatrix):
    """
    Let delta = min(c_{i j}) where {i not in V} and {j not in V}
    (i, j) are (row, col) indices.
    """
    N = len(costmatrix)
    V = _konig(costmatrix)
    delta = min(costmatrix[i][j] for i in xrange(N) for j in xrange(N)
                if (i not in V) or (j not in V))

    for i in xrange(N):
        for j in xrange(N):
            if (i not in V) and (j not in V):
                costmatrix[i][j] -= delta
            elif (i in V) and (j in V):
                costmatrix[i][j] += delta
            else:
                pass
    return costmatrix


def find_max_ss(clist, plist):
    """
    Find the maximum suitability score (SS) from all combinations
    of customers and products. Then return the maximum score.
    """
    perm = itertools.permutations
    izip = itertools.izip

    ## Prepare SS table
    scores = [[suitability_score(c, p) for p in plist] for c in clist]
    num_customers = len(clist)
    num_products = len(plist)

    ## Find the best assignment
    cindices = range(num_customers)
    pindices = range(num_products)
    if num_products <= num_customers:
        return max(sum(scores[i][j] for (i, j) in izip(cselected, pindices))
                   for cselected in perm(cindices, num_products))
    else:
        return max(sum(scores[i][j] for (i, j) in izip(cindices, pselected))
                   for pselected in perm(pindices, num_customers))


def read_data(line):
    (customers, products) = line.rstrip().split(";")
    customers = set(customers.split(","))
    products = set(products.split(","))
    return (customers, products)


def test_hungarian():
    cost = [[1, 4, 5], [5, 7, 6], [5, 8, 8]]
    out = _hungarian_step0(cost)
    print out

    print cost


def test():
    EPSILON = 0.000001
    t0 = time.time()

    customers = "Jack Abraham,John Evans,Ted Dziuba".split(',')
    products = "iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow".split(',')
    assert abs(find_max_ss(customers, products) - 21.00) < EPSILON

    customers = ("Jeffery Lebowski,Walter Sobchak,Theodore Donald Kerabatsos,"
               + "Peter Gibbons,Michael Bolton,Samir Nagheenanajar").split(',')
    products = ("Half & Half,Colt M1911A1,16lb bowling ball,Red Swingline Stapler,"
               + "Printer paper,Vibe Magazine Subscriptions - 40 pack").split(',')
    assert abs(find_max_ss(customers, products) - 83.50) < EPSILON

    customers = ("Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson").split(',')
    products = ("Batman No. 1,Football - Official Size,Bass Amplifying Headphones,"
              + "Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,"
              + "Dom Perignon 2000 Vintage").split(',')
    assert abs(find_max_ss(customers, products) - 71.25) < EPSILON

    customers = "Jack Abraham,John Evans,Ted Dziuba,Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson".split(',')
    products = "Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage,iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow".split(',')
    assert abs(find_max_ss(customers, products) - 96.50) < EPSILON

    t1 = time.time()
    print "time:", t1 - t0
    print "passed all tests!"

if __name__ == "__main__":
    test_hungarian()
    # with open(sys.argv[1], "r") as f:
    #     data = [read_data(s) for s in f if s.rstrip()]

    # out = [find_max_ss(customers, products) for (customers, products) in data]
    # for (customers, products) in data:
    #     print (len(customers), len(products))

    # print "\n".join("%.2f" % x for x in out)
