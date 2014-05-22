-- HASKELL BASICS - extra notes about function composition


-- Another useful higher-order function is for doing function composition.
-- This is defined in the Prelude as the infix operator .

-- First, let's repeat these two definitions to have something to try:

incr x = x+1
double x = 2*x

-- now try evaluating : (double . incr) 10

-- We can also use function composition in a definition:
f = double . incr

-- You could define it yourself:
compose f g x = f (g x)

{- What is the type of compose??  Try it and see what Haskell says for
  :t compose
or
  :t (.)
-}


{- OK, the type of (.) is 

(.) :: (b -> c) -> (a -> b) -> a -> c

Notice that the two functions don't need to have exactly the same type -- 
rather, the argument type of the first has to be the return type of the 
second. 

Example:

(const True) . incr

has type Integer -> Bool

-}