{- Assignment 1
Chun-Wei Chen
CSE 341
09/30/12
-}

module Assignment_1
	where

import Data.Char

-- This function takes two arguments (radius and height) to calculate the corresponding cone volume. 
cone_volume :: Double -> Double -> Double
cone_volume r h = pi * r * r * h / 3.0

-- This function takes a list of integers and returns a list of the squares of those integers.
-- This is a recursive function.
squares :: [Int] -> [Int]
squares [] = []
squares (a:b) = a * a : squares b

-- This function takes a list of integers and returns a list of the squares of those integers.
-- This function uses map instead of using recursive function, and it's an anonymous function.
map_squares :: [Int] -> [Int]
map_squares a = map (\x -> x * x) a

-- This is the pointfree version of square function.
pointfree_squares :: [Int] -> [Int]
pointfree_squares = map (^2)

-- This function takes a list of integers and determine if it is in the strict ascending order.
-- If number on the right equals to the number on the left, the function returns false.
ascending :: [Int] -> Bool
ascending [] = True -- There's nothing to compare, so so it won't be wrong to say it's ascending.
ascending [a] = True -- There's only one value, so it won't be wrong to say it's ascending.
ascending (a:b) = 
	if a < head b
		then ascending b
		else False
{- Another version of ascending
ascending [] = True
ascending [_] = True
ascending (x:y:zs) = x < y && ascending (y:zs)
-}

-- This function calculates the total resistance of a number of resistors connected in parallel.
-- Pass in a empty list will get Infinity.
-- Pass in a 0, or a list of 0 will return 0.0 since 1/(1/0) == 0 and 1/((1/0)+(1/0) are both true.
parallel_resistors :: [Double] -> Double
parallel_resistors x = recip (sum (map recip x))

-- This is the pointfree version of parallel_resistors function.
pointfree_parallel_resistors :: [Double] -> Double
pointfree_parallel_resistors = (1/) . sum . map (1/)

-- This function takes a string (a list of characters) and determine whether it is a palindrome.
-- When it determine if the string is palindrome, it only considers 'A'..'Z', 'a'..'z' and '0'..'9'.
-- This function is not case-sensitive.
palindrome :: [Char] -> Bool
palindrome "" = True
palindrome (a:b) =
	let s = map toUpper (filter_for_palindrome (a:b))
	in s == reverse s

-- This is a helper function which helps palindrome function filter out 
-- the characters other than letters or digits in the string passed in.	
filter_for_palindrome =
	let it = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']
	in filter (`elem` it)