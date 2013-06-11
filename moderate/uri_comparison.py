#!/usr/bin/env python
# encoding: utf-8
"""
URI Comparison

Description:

Determine if two URIs match. For the purpose of this challenge, you should use a case-sensitive octet-by-octet comparison of the entire URIs, with these exceptions:

1. A port that is empty or not given is equivalent to the default port of 80
2. Comparisons of host names MUST be case-insensitive
3. Comparisons of scheme names MUST be case-insensitive
4. Characters are equivalent to their % HEX HEX encodings. (Other than typical reserved characters in urls like /,?,@,:,&,= etc)

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains two urls delimited by a semicolon. e.g.

http://abc.com:80/~smith/home.html;http://ABC.com/%7Esmith/home.html

Output sample:

Print out True/False if the URIs match. e.g.

True
"""
import sys
from urllib2 import unquote
from urlparse import urlparse

def compare_uri(s, t):
    """
    Excluding username and password 
    """
    sss = urlparse(unquote(s))
    ttt = urlparse(unquote(t))

    for x in (sss, ttt):
        try:
            tmp = x.port if x.port else 80
        except ValueError:
            tmp = x.netloc.split(':')[-1]
            tmp = tmp if tmp else 80
        x.myport = tmp

    return (sss.scheme.lower() == ttt.scheme.lower() and
        sss.hostname.lower() == ttt.hostname.lower() and
        sss.username == ttt.username and
        sss.password == ttt.password and
        sss.path == ttt.path and
        sss.myport == ttt.myport )


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [line.rstrip().split(';') for line in f]

    for (s, t) in data:
        print compare_uri(s, t)
