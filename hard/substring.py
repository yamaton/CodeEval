#!/usr/bin/env python
# encoding: utf-8
"""
String Searching
================
Created by Yamato Matsuoka on 2012-07-18.

Description
------------
You are given two strings. Determine if the second string is a substring of the first (Do NOT use any substr type library function). The second string may contain an asterisk(*) which should be treated as a regular expression i.e. matches zero or more characters. The asterisk can be escaped by a \ char in which case it should be interpreted as a regular '*' character. To summarize: the strings can contain alphabets, numbers, * and \ characters.

Input sample
------------
File containing two comma delimited strings per line. e.g.
```
Hello,ell
This is good, is
CodeEval,C*Eval
Old,Young
```

Output sample
-------------
If the second string is indeed a substring of the first, print out a 'true'(lowercase), else print out a 'false'(lowercase), one per line. e.g.
```
true
true
true
false
```

"""

import sys
import re


def toRegex(s):
    if '\*' in s:
        return s
    elif '*' in s:
        return s.replace('*', '.*')
    else:
        return s


def containedQ(string, pattern):
    return True if re.search(toRegex(pattern), string) else False


def format(tf):
    return str(tf).lower()


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[x for x in line.rstrip().split(',')] for line in f if line.rstrip()]
    out = (containedQ(s, patt) for (s, patt) in data)
    print "\n".join(format(x) for x in out)
