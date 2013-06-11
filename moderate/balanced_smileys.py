#!/usr/bin/env python
# enconding: utf-8
"""
Created by Yamato on 2013-02-07

Balanced Smileys

Description:

Credits: This problem appeared in the Facebook Hacker Cup 2013 Hackathon.

Your friend John uses a lot of emoticons when you talk to him on Messenger. In addition to being a person who likes to express himself through emoticons, he hates unbalanced parenthesis so much that it makes him go :(.

Sometimes he puts emoticons within parentheses, and you find it hard to tell if a parenthesis really is a parenthesis or part of an emoticon. A message has balanced parentheses if it consists of one of the following:

- An empty string ""
- One or more of the following characters: 'a' to 'z', ' ' (a space) or ':' (a colon)
- An open parenthesis '(', followed by a message with balanced parentheses, followed by a close parenthesis ')'.
- A message with balanced parentheses followed by another message with balanced parentheses.
- A smiley face ":)" or a frowny face ":("

Write a program that determines if there is a way to interpret his message while leaving the parentheses balanced.

Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains a message that you got from John. e.g.

:((
i am sick today (:()
(:)
hacker cup: started :):)
)(

Output sample:

Print out the string "YES"/"NO"(all quotes for clarity only) stating whether or not it is possible that the message had balanced parentheses. e.g.

NO
YES
YES
YES
NO


[LOG]
    - v0.02  Allow nested pharenthesis structure now
    - v0.01  created
"""

import sys
import re
import itertools


def if_balanced(s):
    regex = re.compile(r"[^a-z \(\):]")
    if not s:
        ## Check if the string s is blank
        return "YES"
    elif regex.search(s):
        ## Check if the string s contains letters except a-z, space, :, and ( , )
        return "NO"

    regex = re.compile(r":\)|:\(")
    positions = [m.span() for m in regex.finditer(s)]
    number_of_possible_emoticons = len(positions)

    tf = any(if_parenthesis_closed(truncate(s, spans))
        for n in range(number_of_possible_emoticons + 1)
        for spans in itertools.combinations(positions, n))

    return "YES" if tf else "NO"


def truncate(s, spans):
    if not spans:
        return s

    indices = [i for j in spans for i in j]   # flatten
    head = s[:indices[0]]
    tail = s[indices[-1]:]
    body = ""
    if len(indices) > 2:
        ## reshape 1-dim array into 2-dim
        remaining = [indices[i:i+2] for i in range(1, len(indices)-1)[::2]]
        body = "".join(s[start:end] for (start, end) in remaining)
    return head + body + tail


def if_parenthesis_closed(s):
    stack = 0
    for c in s:
        if c == "(":
            stack += 1
        elif c == ")":
            stack -= 1
            if stack < 0:
                return False
    return False if stack else True


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [s.rstrip() for s in f]

    for s in data:
        print if_balanced(s)
