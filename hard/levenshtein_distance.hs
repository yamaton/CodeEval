{-
levenshtein_distance.hs

Created by Yamato Matsuoka on 2013-07-12.

Two words are friends if they have a Levenshtein distance of 1 (For details see http://en.wikipedia.org/wiki/Levenshtein_distance). That is, you can add, remove, or substitute exactly one letter in word X to create word Y. A word’s social network consists of all of its friends, plus all of their friends, and all of their friends’ friends, and so on. Write a program to tell us how big the social network for the word 'hello' is, using this word list https://raw.github.com/codeeval/Levenshtein-Distance-Challenge/master/input_levenshtein_distance.txt

Input sample
-------------
Your program should accept as its first argument a path to a filename. The input file contains the word list. This list is also available at https://raw.github.com/codeeval/Levenshtein-Distance-Challenge/master/input_levenshtein_distance.txt.

Output sample
---------------
Print out how big the social network for the word 'hello' is. e.g. The social network for the word 'abcde' is 4846.


[Comments]

The code gives "hello" has 4844 friends.
But computation takes too much time: 10 sec with pypy and 45 sec with python.

===> By introducing Prefix and Suffix Dictionary, the runtime is shrunk to 13.5 sec (8.7sec with pypy).

Introduced list of distance of a word from the initial word (say, 'hello') and used it to limit the population but it was no use...


-}

import System.Environment (getArgs)


def levenshtein(s, t):
    """
    Compute Levenshtein distance between string s and t.
    """
    len1, len2 = len(s), len(t)
    if len1 == 0:
        return len2
    elif len2 == 0:
        return len1

    cost = 0 if s[-1] == t[-1] else 1
    return min(levenshtein(s[:-1], t) + 1,
               levenshtein(s, t[:-1]) + 1,
               levenshtein(s[:-1], t[:-1]) + cost)


def is_friend1(s, t):
    """Check if two strings (s, t) are in Levinshetein distance 1."""
    assert len(s) == len(t)
    izip = itertools.izip
    diff_count = sum(schar != tchar for (schar, tchar) in izip(s, t))

    ## If s and t are identical s == t, they are not friends.
    return diff_count == 1


def is_friend2(s, t):
    """Check if two strings (s, t) are in Levinshetein distance 1."""
    assert len(s) == len(t) + 1
    for i, _ in enumerate(t):
        if s[i] != t[i]:
            return s[i+1: ] == t[i: ]
    else:
        ## This is the case like s, t = "jellos", "jello"
        return True


def createDictionaries(words):
    LenDict = {}
    PrefixDict = {}
    SuffixDict = {}
    for w in words:
        l = len(w)
        p = w[0]
        s = w[-1]
        if l in LenDict:
            LenDict[l].add(w)
        else:
            LenDict[l] = set([w])
        if p in PrefixDict:
            PrefixDict[p].add(w)
        else:
            PrefixDict[p] = set([w])
        if s in SuffixDict:
            SuffixDict[s].add(w)
        else:
            SuffixDict[s] = set([w])
    return LenDict, PrefixDict, SuffixDict


def friend_network(iniword, words):
    LenDict, PrefixDict, SuffixDict = createDictionaries(words)

    stack = [iniword]
    friends = set([iniword])
    while stack:
        t = stack.pop()
        lenT = len(t)
        PS = PrefixDict[t[0]] | SuffixDict[t[-1]]
        new_friends = set(s for s in (LenDict[lenT] & PS)
                         if (s not in friends) and is_friend1(t, s))
        if (lenT + 1) in LenDict:
            new_friends.update(s for s in (LenDict[lenT + 1] & PS)
                         if (s not in friends) and is_friend2(s, t))
        if (lenT - 1) in LenDict:
            new_friends.update(s for s in (LenDict[lenT - 1] & PS)
                         if (s not in friends) and is_friend2(t, s))

        if new_friends:
            friends.update(new_friends)
            stack.extend(new_friends)

    ## iniword may be excluded from the network
    return len(friends) - int(iniword not in words)


def test():
    assert is_friend1("hello", "mello") is True
    assert is_friend1("hello", "heloo") is True
    assert is_friend1("hello", "hellp") is True
    assert is_friend1("hello", "helol") is False

    assert is_friend2("hello", "hell") is True
    assert is_friend2("hello", "hall") is False
    assert is_friend2("hello", "ello") is True
    assert is_friend2("hello", "helo") is True
    assert is_friend2("hello", "hllo") is True
    assert is_friend2("hello", "helo") is True
    assert is_friend2("helloou", "haloou") is False
    print "passed all tests!"


if __name__ == '__main__':
    ## [debug] -------- BEGIN ------------
    # test()
    # with open("input_levenshtein_distance.txt", "r") as f:
    #     words = set(line.rstrip() for line in f if line.rstrip())
    # print "# of words:", len(words)
    ## [debug] --------- END -------------

    with open(sys.argv[1], "r") as f:
        words = [s.rstrip() for s in f if s.rstrip()]

    ## w = 'abcde'
    w = 'hello'
    print friend_network(w, words)
