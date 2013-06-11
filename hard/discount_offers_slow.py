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


Ver 0.10 Brute-force with memorization. Works for the sample inputs but too slow.

"""
import time
import sys
import itertools

def memorize(f):
    table = {}
    def func(*args):
        if not args in table:
            table[args] = f(*args)
        return table[args]
    return func


def gcd(a,b):
    """Using Euclid's algorithm"""
    if b > a:   a, b = b, a
    if b == 0:
        return a
    else:
        return gcd(b, a - b * (a//b))


@memorize
def suitability_score(c_name, p_name):
    num_letters_customer = len([ i for i in c_name if i.isalpha() ])
    num_vowels_customer = len([ i for i in c_name.lower() if i in 'aeiouy'])
    num_letters_product =  len([ i for i in p_name if i.isalpha() ])

    if num_letters_product % 2 == 0:
        ss = 1.5 * num_vowels_customer
    else:
        ss = num_letters_customer - num_vowels_customer
    
    if gcd(num_letters_customer, num_letters_product) > 1:
        ss *= 1.5
    return ss


def find_max_ss(cset, pset):
    imap = itertools.imap
    comb = itertools.combinations
    perm = itertools.permutations
    f = suitability_score
    num_products = len(pset)

    # brute force method
    return max(sum(imap(f, cselected, pset))
            for cselected in perm(cset, num_products))


def read_data(line):
    (customers, products) = line.rstrip().split(";")
    customers = customers.split(",")
    products =  products.split(",")
    return (customers, products)




EPSILON = 0.000001

def test():

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
    test()
    # with open(sys.argv[1], "r") as f:
    #     data = [read_data(s) for s in f if s.rstrip()]
    # out = [scan_for_maxss(customers, products)
    #             for (customers,products) in data]
    # print "\n".join("%.2f" % x for x in out)
