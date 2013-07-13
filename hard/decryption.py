"""
For this challenge you are given an encrypted message and a key. You have to determine the encryption and encoding technique and then print out the corresponding plaintext message. You can assume that the plaintext corresponding to this message, and all messages you must handle, will be comprised of only the characters A-Z and spaces; no digits or punctuation.

Input sample:

There is no input for this program. The encrypted message and key is:

message: "012222 1114142503 0313012513 03141418192102 0113 2419182119021713 06131715070119",
keyed_alphabet: "BHISOECRTMGWYVALUZDNFJKPQX"
Output sample:

Print out the plaintext message. (in CAPS)
"""

import string


def int2chr(n, k=0):
    return chr((n-k) + ord('A'))


def split_every_two(s, n=2):
    """Split string into every n letters"""
    return [int(s[n*i:n*i+n]) for i in range(len(s)/n)]


if __name__ == '__main__':
    message = "012222 1114142503 0313012513 03141418192102 0113 2419182119021713 06131715070119".split()
    key = "BHISOECRTMGWYVALUZDNFJKPQX"
    table1 = string.maketrans(key, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    table2 = string.maketrans("ABCDEFGHIJKLMNOPQRSTUVWXYZ", key)

    message = [split_every_two(s) for s in message]
    # print message
    message = ["".join(map(int2chr, s)) for s in message]
    # print message

    print " ".join(m.translate(table1) for m in message)
    # print " ".join(m.translate(table2) for m in message)
