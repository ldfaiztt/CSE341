{- 

HASKELL - LAZY EVALUATION AND INFINITE DATA STRUCTURES
CSE 341

-}

module InfiniteDataStructures
    where

import Data.Char

{- Since Haskell uses lazy evaluation, we can have infinite data structures --
we only produce as much of the structure as is needed.  We've seen various
examples of this, for example
    [1.. ]
    [1,3 ..]
    verylargetree (from TypesNotes.hs)

Why Infinite Lists?

Are infinite lists (and other infinite data structures) simply curiosities
or something more important?

Simon Thompson in "The Craft of Functional Programming" argues that they
are more important, for two reasons:

First, an infinite version of a program can be more abstract and simpler to
write, since we don't need to know in advance how long to make the list
(e.g. the fibonacci number list).

Second, we can often modularize the generation of values, and separate out
some transformations we perform on them.  We can then link process-style
programs that involve recursion.  (Thompson gives a simulation example.)

-}

my_member :: Eq a => a -> [a] -> Bool
my_member x []    = False
my_member x (y:z) | x==y      = True
                  | otherwise = my_member x z

-- this works:  my_member 10 [1..]
-- this searches forever:  my_member 3 [10..]

-- MEMBER FUNCTION FOR SORTED LISTS

sorted_member :: Ord a => a -> [a] -> Bool
sorted_member x [] = False
sorted_member x (y:z) | x==y    = True
                      | x<y     = False
                      | x>y     = sorted_member x z


{- this version of member will always terminate.  For example
  sorted_member 3 [10..]
terminates immediately
-}



-- SQUARE ROOT 


sqt x =  head (filter close_enough (approximations x))
         where  approximations a = a : approximations ((a + x/a)/2)
		close_enough root = abs (x-root*root) < 1.0e-6


-- to show the list of successive approximations:
trial_roots x = approximations x
                where  approximations a = a : approximations ((a + x/a)/2)

-- INFINITE LIST OF PRIME NUMBERS
factors n = [k | k <- [1..n], n `mod` k == 0]

dullprimes = filter isprime [2..]
             where
             isprime p = (factors p == [1,p])


{- Fibonacci numbers (clear but inefficient version, with a nasty 
   double recursion ) -}
rec_fib 1 = 1
rec_fib 2 = 1
rec_fib n = rec_fib (n-1) + rec_fib (n-2)

slowfibs = map rec_fib [1..]


{- map2 is like map but takes a 2-argument function (we'll use it 
   for another version of Fibonacci numbers). If the lists are of 
   different lengths we just use as many elements as there are in 
   the shorter list. -}

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
map2 _ _ _ = []


{- Better version of the list of Fibonacci numbers.  This uses a
   common pattern for Haskell infinite list definitions, in which you have
   a prime-the-pump sort of recursive definition.-}
fibs = 1 : 1 : map2 (+) fibs (tail fibs)


{- HAMMING NUMBERS

The Hamming numbers are defined as the ordered list of all numbers of the form
   2^a * 3^b * 5^c
for non-negative integers a, b, and c; in other words, numbers whose
only prime factors are 2, 3. and 5. -}


my_merge (x:a) (y:b) | x<y   = x : my_merge a (y:b)
                     | x==y  = y : my_merge a b 
                     | x>y   = y : my_merge (x:a) b

ham = 1: my_merge ham2 (my_merge ham3 ham5)

ham2 = map (*2) ham
ham3 = map (*3) ham
ham5 = map (*5) ham

{- 
    take 100 ham 
evaluates to:

[1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36,
40, 45, 48, 50, 54, 60, 64, 72, 75, 80, 81, 90, 96, 100, 108, 120, 125,
128, 135, 144, 150, 160, 162, 180, 192, 200, 216, 225, 240, 243, 250, 256,
270, 288, 300, 320, 324, 360, 375, 384, 400, 405, 432, 450, 480, 486, 500,
512, 540, 576, 600, 625, 640, 648, 675, 720, 729, 750, 768, 800, 810, 864,
900, 960, 972, 1000, 1024, 1080, 1125, 1152, 1200, 1215, 1250, 1280, 1296,
1350, 1440, 1458, 1500, 1536]

-}

{- Infinite list of prime numbers using the Sieve of Eratosthenes.
See http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes -}

interestingprimes = sieve [2..]
                    where
                    sieve (p:x) = p : sieve [n | n <- x, n `mod` p > 0]


{- 
Haskell program to compute e (the base of natural logarithms) to an 
infinite number of decimal places.  Adapted from an example by 
D.A. Turner, "Recursion Equations as a Programming Language", in 
"Functional Programming and its Applications: An Advanced Course", 
Cambridge, 1982.
-}


-- We'll represent e as an infinite string "2.71828....."
e :: String
e_digits :: [Int]  -- the list of digits in e

{- e is equal to the limit of the sum 1/0! + 1/1! + 1/2! + 1/3! + ...
Since we want an indefinitely large number of digits, we can't just use the
builtin Double type and sum up terms until we converge to the limit of the
precision of Double.  Instead we observe that e is equal to
2.1111111111111111111 ....  written in a peculiar base, where the weight of
the digit i is i! (Note that the "carry factor" from the i-th digit back to
the (i-1)st is i.)  -}

e_weird_base = 2 : ones
ones = 1 : ones

-- e_digits is the result of converting e written in the weird base to
-- decimal notation
e_digits = convert e_weird_base

{- Turner says: The general algorithm for converting from any base to
decimal can be stated in words as follows.  First print the integer part of
the number, then take the remaining digits, multiply them all by 10 and
renormalize (using the appropriate carry factors) - the new integer part
will be the next decimal digit and this process can be repeated
indefinitely.  We can capture this in a purely functional way by
representing the number before and after conversion as two infinite lists,
and defining a function "convert" recursively. -}

-- convert from the weird base to decimal
convert :: [Int] -> [Int]
convert (d:x) = d : convert (normalize 2 (0 : map (*10) x))

{- Helper function to carry from the second digit to the first in a
   (possibly infinite) list of digits.  This only deals with the first
   and second digits, even if additional carries are needed later.  To
   see how this works on a base 10 example, try 
      carry 10 [2,13,4,1,4,15,5]   -}
carry :: Int -> [Int] -> [Int]
carry c (d:e:x) = d + div e c : mod e c : x

-- OK, now the normalize function
normalize :: Int -> [Int] -> [Int]

{- Turner again: a first cut at normalize might be

normalize c (d:x) = carry c (d : normalize (c+1) x)

However, trying to evaluate this will lead to printing 2, followed by a
long silence [actually a stack overflow when I tried it].  The problem is
that the recursion for "normalize" is not well-founded (it tries to look
infinitely far to the right before producing the first digit).  We need
some result which can limit the distance from which a carry can propagate,
or else we are stuck.  (This is a logical problem, not just a coding
difficulty.)

The necessary cut-off rule is provided by the observation that in the above
conversion the maximum possible carry from a digit to its immediately
leftward neighbor is 9.  This is reflected in the following version of
"normalize": -}

normalize c (d:e:x) 
    | e+9<c  = d : normalize (c+1) (e:x)
    | otherwise = carry c (d : normalize (c+1) (e:x))

e = show (head e_digits) ++ "." ++ map intToDigit (tail e_digits)