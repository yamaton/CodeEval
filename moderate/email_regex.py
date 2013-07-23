#!/usr/bin/env python
# encoding: utf-8
"""
email_regex.py

Created by Yamato Matsuoka on 2012-07-17.

Description
-----------
You are given several strings that may/may not be valid emails. You should write a regular expression that determines if the email id is a valid email id or not. You may assume all characters are from the english language.

Input sample
-------------
Your program should accept as its first argument a filename. This file will contain several text strings, one per line. Ignore all empty lines. eg.
```
foo@bar.com
this is not an email id
admin#codeeval.com
good123@bad.com
```

Output sample
--------------
Print out 'true' (all lowercase) if the string is a valid email. Else print out 'false' (all lowercase) .e.g.
```
true
false
false
true
```

"""

import sys
import re

REGEX = "[A-Za-z0-9._+-]+@[A-Za-z0-9.-]+\.([a-zA-Z]){2,4}"
PROG = re.compile(REGEX)


def is_email_address(s):
    return "true" if PROG.match(s) else "false"

if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [s.rstrip() for s in f]
    out = (is_email_address(s) for s in data)
    for i in out:
        print i
