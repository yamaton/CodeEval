{-
sum_prime.hs

Created by Yamato Matsuoka on 2013-07-03.

Description: Write a program to determine the sum of the first 1000 prime numbers.

Input sample: None

Output sample:  Your program should print the sum on stdout.i.e.

-}


-- This is too slow!
sieve :: [Int] -> [Int]
sieve (x:xs) = x: filter (\n -> mod n x /= 0) (sieve xs)


primes :: Int -> [Int]
primes n
    | n >= 6    = let logN = log $ fromIntegral n
                      upperbound = (round $ n * logN + log logN) :: Int
                  in sieve upperbound
    | otherwise = [2, 3, 5, 7, 11] !! n

main = do 
    print (sum $ take 1000 $ primes)

