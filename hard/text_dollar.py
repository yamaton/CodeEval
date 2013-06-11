#!/usr/bin/env python
# encoding: utf-8
"""
text_dollar.py

Created by Yamato Matsuoka on 2012-07-19.

Description:

Credits: This challenge has been authored by Terence Rudkin

You are given a positive integer number. This represents the sales made that day in your department store. The payables department however, needs this printed out in english. NOTE: The correct spelling of 40 is Forty. (NOT Fourty)

Input sample:

Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains a positive integer. eg.

3
10
21
466
1234

Output sample:

For each set of input produce a single line of output which is the english textual representation of that integer. The output should be unspaced and in Camelcase. Always assume plural quantities. You can also assume that the numbers are < 1000000000 (1 billion). In case of ambiguities eg. 2200 could be TwoThousandTwoHundredDollars or TwentyTwoHundredDollars, always choose the representation with the larger base i.e. TwoThousandTwoHundredDollars. For the examples shown above, the answer would be:

ThreeDollars
TenDollars
TwentyOneDollars
FourHundredSixtySixDollars
OneThousandTwoHundredThirtyFourDollars

"""

import sys

def text_dollar(x):
    """Textify a number x < 10**9"""

    millions  = x // 1000000
    x = x % 1000000
    thousands = x // 1000
    x = x % 1000
    
    out = []
    if millions > 0:
        out.append(_textify_number_hundreds(millions) + "Million")
    if thousands > 0:
        out.append(_textify_number_hundreds(thousands) + "Thousand")
    out.append(_textify_number_hundreds(x))
    
    if x == 1:
        return "".join(out) + "Dollar"
    else:
        return "".join(out) + "Dollars"


def _textify_number_hundreds(x):
    d_tens = {20:"Twenty", 30:"Thirty", 40:"Forty", 50:"Fifty", 
              60:"Sixty", 70:"Seventy", 80:"Eighty", 90:"Ninety"}
    d_ones = {10:"Ten", 11:"Eleven", 12:"Twelve", 13:"Thirteen",
              14:"Fourteen", 15:"Fifteen", 16:"Sixteen", 17:"Seventeen",
              18:"Eighteen", 19:"Nineteen",
               1:"One", 2:"Two", 3:"Three", 4:"Four", 5:"Five", 
               6:"Six", 7:"Seven", 8:"Eight", 9:"Nine"}
    hundreds = x // 100
    x = x % 100
    if x < 20:
        tens = 0
        ones = x
    else:
        tens = x // 10
        ones = x % 10
    
    out = []
    if hundreds > 0:
        out.append(d_ones[hundreds] + "Hundred")
    if tens > 0:
        out.append(d_tens[tens*10])
    if ones > 0:
        out.append(d_ones[ones])
    return "".join(out)



if __name__ == '__main__':
    with open(sys.argv[1], "r") as f:
        seq = [int(s) for s in f if s.rstrip()]
    
    out = [text_dollar(x) for x in seq]
    print "\n".join(out)
