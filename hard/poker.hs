{-
Poker hands
============
Description
-----------
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way: 

High Card: Highest value card. 
One Pair: Two cards of the same value. 
Two Pairs: Two different pairs. 
Three of a Kind: Three cards of the same value. 
Straight: All cards are consecutive values. 
Flush: All cards of the same suit. 
Full House: Three of a kind and a pair. 
Four of a Kind: Four cards of the same value. 
Straight Flush: All cards are consecutive values of same suit. 
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit. 
The cards are valued in the order: 
2, 3, 4, 5, 6, 7, 8, 9, Ten, Jack, Queen, King, Ace. 

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives. But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared; if the highest cards tie then the next highest cards are compared, and so on. 

Input sample
-------------
Your program should accept as its first argument a path to a filename. Each line in this file contains 2 hands (left and right). Cadrs and hands are separated by space. E.g.
```
6D 7H AH 7S QC 6H 2D TD JD AS
JH 5D 7H TC JS JD JC TS 5S 7S
2H 8C AD TH 6H QD KD 9H 6S 6C
JS JH 4H 2C 9H QH KC 9D 4D 3S
TC 7H KH 4H JC 7D 9S 3H QS 7S
```

Output sample
--------------
Print out the name of the winning hand or "none" in case the hands are equal. E.g.
```
left
none
right
left
right
```

[Credit] The original code is from Udacity Course CS212 Unit 1.
-}

import System.Environment (getArgs)

type Hand -> [String]

poker :: Hand -> Hand -> String
poker hand1 hand2 =
  | length outcome > 1    = "none"
  | head outcome == hand1 = "left"
  | otherwise             = "right"
    where outcome = allMax handRank hand1 hand2

allMax :: Ord a => [a] -> 


def allmax(iterable, key=None):
    "Return a list of all items equal to the max of the iterable."
    onemax = max(iterable, key=key)
    key = key or (lambda x: x)
    return [x for x in iterable if key(x) == key(onemax)]


def hand_rank(hand):
    "Return a value indicating the ranking of a hand."
    ranks = card_ranks(hand)
    if straight(ranks) and flush(hand):
        return (8, max(ranks))
    elif kind(4, ranks):
        return (7, kind(4, ranks), kind(1, ranks))
    elif kind(3, ranks) and kind(2, ranks):
        return (6, kind(3, ranks), kind(2, ranks))
    elif flush(hand):
        return (5, ranks)
    elif straight(ranks):
        return (4, max(ranks))
    elif kind(3, ranks):
        return (3, kind(3, ranks), ranks)
    elif two_pair(ranks):
        return (2, two_pair(ranks), ranks)
    elif kind(2, ranks):
        return (1, kind(2, ranks), ranks)
    else:
        return (0, ranks)


def card_ranks(hand):
    "Return a list of the ranks, sorted with higher first."
    ranks = ['--23456789TJQKA'.index(r) for r, s in hand]
    ranks.sort(reverse=True)
    return [5, 4, 3, 2, 1] if (ranks == [14, 5, 4, 3, 2]) else ranks


def flush(hand):
    "Return True if all the cards have the same suit."
    suits = [s for (r, s) in hand]
    return len(set(suits)) == 1


def straight(ranks):
    "Return True if the ordered ranks form a 5-card straight."
    return (max(ranks) - min(ranks) == 4) and len(set(ranks)) == 5


def kind(n, ranks):
    """Return the first rank that this hand has exactly n-of-a-kind of.
    Return None if there is no n-of-a-kind in the hand."""
    for r in ranks:
        if ranks.count(r) == n: return r
    return None


def two_pair(ranks):
    "If there are two pair here, return the two ranks of the two pairs, else None."
    pair = kind(2, ranks)
    lowpair = kind(2, list(reversed(ranks)))
    if pair and lowpair != pair:
        return (pair, lowpair)
    else:
        return None


def read_data(s):
    hands = s.rstrip().split(" ")
    return (hands[:5], hands[5:])


if __name__ == '__main__':
    with open(sys.argv[1], 'r') as f:
        data = [read_data(s) for s in f if s.rstrip()]
    for (left, right) in data:
        print poker(left, right)
