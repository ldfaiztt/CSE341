{- 
What's going on with 'do' in Haskell?
CSE 341
-}

module DesugaringDo
    where

{- The 'do' syntax in Haskell makes it convenient to write short
input-output actions, but the syntatic sugar may obscure what's going on
underneath.  Here we'll illustrate how to to translate a 'do' statement
into more standard Haskell.  (The process of removing syntatic sugar is
called 'desugaring'.)  We can in fact systematically and mechanically
eliminate "do" from Haskell code, replacing it with monad composition.
This is what the Haskell compiler does -- see Section 9.1 of Yet Another
Haskell Tutorial for the formal rules being used.

Here's the definition of the Monad type class:

class Monad m where 
    return :: a -> m a 
    fail :: String -> m a 
    (>>=) :: m a -> (a -> m b) -> m b 
    (>>) :: m a -> m b -> m b 

We've already seen 'return'.  'fail' takes a string and returns a monad
that indicates the computation failed.  The remaining operators provide a
way to compose two monads to make a new one.

>> (read "then") just sequences two monads:

a >> b

is the action that, if it is ever performed, first does a, then does b.

For example, try:

putStr "Hi " >> putStrLn "there"

We can give this a name, use it in other actions, and so forth:
-}
greet = putStr "Hi " >> putStrLn "there"
manygreetings 0 = return ()
manygreetings n = greet >> manygreetings (n-1)

{-
>>= (read "bind") also sequences two monads, but also feeds the result from
the first to the second:

a>>=b

(Note that b has to be a function that accepts an argument of the correct
type.)

Now let's translate the trig examples from before. -}

{- return a string consisting of a number and its sin and cos -}
trig_facts :: Float -> String
trig_facts x = "x=" ++ show x ++ " sin(x)=" ++ show (sin x) 
               ++ " cos(x)=" ++ show(cos x)

{- first let's translate this action:
simpler_print_one_trig_fact = do
  putStr "Please enter a number: " 
  x <- readLn
  putStrLn (trig_facts x)
-}

simpler_print_one_trig_fact = 
  putStr "Please enter a number: " >> readLn >>= \x -> putStrLn (trig_facts x)

{- and now an action that includes 'let':
print_one_trig_fact = do
  putStr "Please enter a number: " 
  s <- getLine
  let x = read s
  putStrLn (trig_facts x)
-}

print_one_trig_fact = putStr "Please enter a number: " >> getLine >>=
         \s -> let x = read s in putStrLn (trig_facts x)

{- and finally the recursive version:
print_trig_facts = do
  putStr "Please enter a number: " 
  x <- readLn
  putStrLn (trig_facts x)
  putStr "again? "
  ans <- getLine
  if head ans == 'y' then print_trig_facts else return ()

-}

print_trig_facts = 
  putStr "Please enter a number: " >> readLn >>= \x -> putStrLn (trig_facts x)
  >> putStr "again? " >> getLine 
  >>= \ans -> if head ans == 'y' then print_trig_facts else return ()