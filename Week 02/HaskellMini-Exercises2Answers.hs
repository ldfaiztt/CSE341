{- Answers to Haskell Mini Exercises #2 -}

module Mini2
    where

import Data.Char

{- 

The following are correct types for member:

member :: (Ord a) => a -> [a] -> Bool
member :: (Eq a) => a -> [a] -> Bool
member :: (Eq a) => [a] -> [[a]] -> Bool
member :: Bool -> [Bool] -> Bool

This is the most general type for member:

member :: (Eq a) => a -> [a] -> Bool
-}


mystery = 1 : map (*2) mystery 
{- mystery is the list of powers of 2: [1,2,4,8,16,32,64,128,256,512, ... -}

{- infinite list of all integers, ordered in such a way that you can find
   any given integer after searching a finite number of elements -}

ints = 0 : intsfrom 1
intsfrom n = n : (-n) : intsfrom (n+1)

{- or an alternate version: -}
otherints = 0: interleave [1..] [-1, -2 ..]
interleave (x:xs) (y:ys) = x : y : interleave xs ys


{- print the square root of 2.  Converted from

printsqrt2 = do
  putStr "the square root of 2 is "
  putStrLn (show (sqrt 2))
-}

printsqrt2 = putStr "the square root of 2 is " >> putStrLn (show (sqrt 2))

{- read in x and calculate its square root.  Converted from:

calcsqrt = do
  x <- readLn
  putStrLn "calculating the square root of x"
  putStrLn (show (sqrt x))
-}

calcsqrt = readLn >>= \x -> putStrLn "calculating the square root of x" >>
             putStrLn (show (sqrt x))

capitalize = do
  s <- getLine
  putStrLn (map toUpper s)