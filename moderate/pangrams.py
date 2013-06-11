#!/usr/bin/env python
# encoding: utf-8
"""
pangrams.py

Created by Yamato Matsuoka on 2012-07-16.

Description:

The sentence 'A quick brown fox jumps over the lazy dog' contains every single letter in the alphabet. Such sentences are called pangrams. You are to write a program, which takes a sentence, and returns all the letters it is missing (which prevent it from being a pangram). You should ignore the case of the letters in sentence, and your return should be all lower case letters, in alphabetical order. You should also ignore all non US-ASCII characters. In case the input sentence is already a pangram, print out the string NULL

Input sample:

Your program should accept as its first argument a filename. This file will contain several text strings, one per line. Ignore all empty lines. eg.

A quick brown fox jumps over the lazy dog
A slow yellow fox crawls under the proactive dog

Output sample:

Print out all the letters each string is missing in lowercase, alphabetical order .e.g.

NULL
bjkmqz

"""

import sys

def space2null(s):
    if s:
        return s
    else:
        return "NULL"


if __name__ == '__main__':
    
    alphabets = set("abcdefghijklmnopqrstuvwxyz")
    
    with open(sys.argv[1], "r") as f:
        data = (s.rstrip().lower() for s in f)
        data = [set(s) for s in data]
    
    complements = ("".join(sorted(alphabets.difference(s))) for s in data)
    complements = (space2null(s) for s in complements)
    print "\n".join(complements)

