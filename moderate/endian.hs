{-
endian.hs

Created by Yamato Matsuoka on 2013-07-14.

Description
------------
Write a program to print out the endianness of the system.

Input sample
-------------
None

Output sample
--------------
Print to stdout, the endianness, wheather it is LittleEndian or BigEndian. e.g.
```
BigEndian
```
-}

import System.Environment (getArgs)

def endian():
    x = sys.byteorder
    if x == "little":
        return "LittleEndian"
    else:
        return "BigEndian"

if __name__ == '__main__':
	print endian()

