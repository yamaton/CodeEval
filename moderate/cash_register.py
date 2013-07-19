#!/usr/bin/env python
# encoding: utf-8
"""
Cash Register
==============
Created by Yamato Matsuoka on 2012-07-16.

Description
-----------
The goal of this challenge is to design a cash register program. You will be given two float numbers. The first is the purchase price (PP) of the item. The second is the cash (CH) given by the customer. Your register currently has the following bills/coins within it:

'PENNY': .01,
'NICKEL': .05,
'DIME': .10,
'QUARTER': .25,
'HALF DOLLAR': .50,
'ONE': 1.00,
'TWO': 2.00,
'FIVE': 5.00,
'TEN': 10.00,
'TWENTY': 20.00,
'FIFTY': 50.00,
'ONE HUNDRED': 100.00

The aim of the program is to calculate the change that has to be returned to the customer.

Input sample
-------------
Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains two numbers which are separated by a semicolon. The first is the Purchase price (PP) and the second is the cash(CH) given by the customer. eg.
```
15.94;16.00
17;16
35;35
45;50
```

Output sample
--------------
For each set of input produce a single line of output which is the change to be returned to the customer. In case the CH < PP, print out ERROR. If CH == PP, print out ZERO. For all other cases print the amount that needs to be returned, in terms of the currency values provided. The output should be sorted in highest-to-lowest order (DIME,NICKEL,PENNY). eg.
```
NICKEL,PENNY
ERROR
ZERO
FIVE
```

"""

import sys


def count_bill_coins(money):
    bill_coins = [10000, 5000, 2000, 1000, 500, 200, 100, 50, 25, 10, 5, 1]
    d = {10000: "ONE HUNDRED", 5000: "FIFTY", 2000: "TWENTY", 
         1000: "TEN", 500: "FIVE", 200: "TWO", 100: "ONE", 50: "HALF DOLLAR",
         25: "QUARTER", 10: "DIME", 5: "NICKEL", 1: "PENNY"}
    
    out = []
    rest = money
    for x in bill_coins:
        while rest >= x:
            out.append(x)
            rest -= x
    return [d[i] for i in out]


def find_change(pp, ch):
    """Return change for given purchase price and cash"""
    pp = int(pp * 100)
    ch = int(ch * 100)
    
    if pp > ch:
        return ["ERROR"]
    elif pp == ch:
        return ["ZERO"]
    else:
        return count_bill_coins(ch - pp)


if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        data = [[float(x) for x in line.rstrip().split(";")] for line in f]
    
    out = (find_change(pp, ch) for (pp, ch) in data)
    print "\n".join(",".join(x) for x in out)

