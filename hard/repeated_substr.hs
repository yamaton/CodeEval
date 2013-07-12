{-
repeated_substr.hs

Created by Yamato Matsuoka on 2013-07-12.

Description
-----------
You are to find the longest repeated substring in a given text. Repeated substrings may not overlap. If more than one substring is repeated with the same length, print the first one you find. (starting from the beginning of the text). NOTE: The substrings can't be all spaces

Input sample
------------
Your program should accept as its first argument a path to a filename. The input file contains several lines. Each line is one test case. Each line contains a test string. eg.
```
banana
```

Output sample
--------------
For each set of input produce a single line of output which is the longest repeated substring. If there is none, print out the string NONE. eg.
```
an
```

Ver 0.21  2012-08-15  [Spec Changed!] Take repeated pattern even if they are separated by characters.
Ver 0.20  2012-08-15  Refactored

banana
cowhollow
hello codeeval
my name is humphery amei
am so uniqe
-}

import System.Environment

def repeated_substr(s, debug=False):
    """Find the longest repeated substring."""
    out = re.findall(r"(?=(.+).*\1)", s)
    ## remove all spaces entry
    out = [i for i in out if not re.match("^\s+$", i)]

    ## For debugging
    if debug:
        print ""
        print "check:", s
        for i in out:
            print i
        print ""

    return max(out, key=len) if out else None


def test():
    assert repeated_substr("____") == "__"
    assert repeated_substr("001") == "0"
    assert repeated_substr("0001111") == "11"
    assert repeated_substr("00111") == "0"
    assert repeated_substr("012012") == "012"
    assert repeated_substr("012") is None
    assert repeated_substr("000qqq") == "0"
    assert repeated_substr("banana") == "an"
    assert repeated_substr("a0000000000bcd") == "00000"
    assert repeated_substr("a          bcd") is None
    assert repeated_substr("a      c     cacd") == "     c"
    assert repeated_substr("babababababa01accacc") == "bababa"
    assert repeated_substr("babababakabakabakab") == "abakab"
    assert repeated_substr("abaababa da8bbaba da8bnd 01aabdc zaka zaka") == "baba da8b"
    print "passed all tests!"


if __name__ == '__main__':
    # test()
    with open(sys.argv[1], "r") as f:
        data = [s.rstrip() for s in f if s.rstrip()]
    out = (repeated_substr(s, debug=True) for s in data)
    for s in out:
        print s

