/*
endian.cpp

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
*/

// http://stackoverflow.com/questions/1001307/detecting-endianness-programmatically-in-a-c-program
int is_big_endian(void)
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    return bint.c[0] == 1; 
}



