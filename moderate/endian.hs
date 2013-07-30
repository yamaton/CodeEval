{-
endian.hs

Created by Yamato Matsuoka on 2013-07-12.

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

[Comment] https://github.com/peti/hsdns/blob/master/ADNS/Endian.hs
-}
import Foreign
import System.IO.Unsafe

data Endian = LittleEndian | BigEndian deriving (Show, Eq)

endian :: Endian
endian = 
    System.IO.Unsafe.unsafePerformIO $
      allocaArray (sizeOf (undefined :: Word32)) $ \p -> do
        let val = 0x01020304 :: Word32
        poke p val
        let p' = castPtr p :: Ptr Word8
        val' <- peekArray 4 p'
        case val' of         
            (0x01:0x02:0x03:0x04:[]) -> return BigEndian
            (0x04:0x03:0x02:0x01:[]) -> return LittleEndian
            _                        -> error "unknown endian"

main = print endian
