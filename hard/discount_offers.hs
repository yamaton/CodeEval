{-
discout_offers.hs
http://codeeval.com/open_challenges/48/

Discount Offers
===============

Description:
------------

Our marketing department has just negotiated a deal with several local merchants that will allow us to offer exclusive discounts on various products to our top customers every day. The catch is that we can only offer each product to one customer and we may only offer one product to each customer.

Each day we will get the list of products that are eligible for these special discounts. We then have to decide which products to offer to which of our customers. Fortunately, our team of highly skilled statisticians has developed an amazing mathematical model for determining how likely a given customer is to buy a given product by calculating what we call the "suitability score" (SS). The top-secret algorithm to calculate the SS between a customer and a product is this:

1. If the number of letters in the product's name is even then the SS is the number of vowels (a, e, i, o, u, y) in the customer's name multiplied by 1.5.
2. If the number of letters in the product's name is odd then the SS is the number of consonants in the customer's name.
3. If the number of letters in the product's name shares any common factors (besides 1) with the number of letters in the customer's name then the SS is multiplied by 1.5.

Your task is to implement a program that assigns each customer a product to be offered in a way that maximizes the combined total SS across all of the chosen offers. Note that there may be a different number of products and customers.

You may include code from external libraries as long as you cite the source.

Input sample:
-------------

Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain a set of comma delimited customer names followed by a semicolon and then a set of comma delimited product names. eg.
```
Jack Abraham,John Evans,Ted Dziuba;iPad 2 - 4-pack,Girl Scouts Thin Mints,Nerf Crossbow

Jeffery Lebowski,Walter Sobchak,Theodore Donald Kerabatsos,Peter Gibbons,Michael Bolton,Samir Nagheenanajar;Half & Half,Colt M1911A1,16lb bowling ball,Red Swingline Stapler,Printer paper,Vibe Magazine Subscriptions - 40 pack

Jareau Wade,Rob Eroh,Mahmoud Abdelkader,Wenyi Cai,Justin Van Winkle,Gabriel Sinkin,Aaron Adelson;Batman No. 1,Football - Official Size,Bass Amplifying Headphones,Elephant food - 1024 lbs,Three Wolf One Moon T-shirt,Dom Perignon 2000 Vintage
```

Output sample:
--------------

For each line of input, print out the maximum total score to two decimal places eg.
```
21.00
83.50
71.25
```

[Comments]
	This is a linear assignment problem.
	http://en.wikipedia.org/wiki/Assignment_problem
	Implement Hungarian algorithm for better performance.

-}

import System.Environment (getArgs)
import Data.Char (isAlpha, toLower)
import Data.List (transpose, permutations)
import Text.Printf (printf)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : split c s''
	where (w, s'') = break (== c) s'

perm :: Int -> [a] -> [[a]]
perm n xs = concatMap permutations $ combinations n xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs  
  | n == 1    = map (:[]) xs 
  | otherwise = helper n (length xs) xs    
	where
	  helper k l ys@(z:zs)        
		| k < l     = map (z :) (combinations (k-1) zs)
						 ++ combinations k zs
		| k == l    = [ys]
		| otherwise = []    

suitabilityScore :: String -> String -> Double
suitabilityScore customers products
  | even numLettersProduct = 1.5 * (modify numVowelsCustomer)
  | otherwise              = modify (numLettersCustomer - numVowelsCustomer)
	where
	  count f xs = length $ filter f xs 
	  numLettersCustomer = count isAlpha customers
	  numLettersProduct  = count isAlpha products
	  numVowelsCustomer =  count (\c -> (toLower c) `elem` "aeiouy") customers
	  multiplier = if (gcd numLettersCustomer numLettersProduct > 1) then (*1.5) else id
	  modify = multiplier . fromIntegral


findMaxSS :: [String] -> [String] -> Double
findMaxSS cList pList
  | lenP <= lenC = maximum [sum (extract scores ns) | ns <- perm lenP [0..(lenC-1)] ]
  | otherwise    = maximum [sum (extract scores' ns) | ns <- perm lenC [0..(lenP-1)] ]
	where 
	  scores = [[suitabilityScore c p | c <- cList] | p <- pList]
	  scores' = transpose scores
	  extract xxs ns = [xxs !! i !! j | (i, j) <- zip [0..(length xxs - 1)] ns]
	  lenC = length cList
	  lenP = length pList


reader :: String -> ([String], [String])
reader s = (customers, products)
  where
	[cs, ps] = split ';' s
	[customers, products] = map (split ',') [cs, ps]


main = do 
  f:_ <- getArgs
  contents <- readFile f
  let inputs = map reader $ filter (not . null) $ lines contents
  let outputs = [findMaxSS cs ps| (cs, ps) <- inputs]
  mapM_ (printf "%.2f\n") outputs
