{- 

INPUT-OUTPUT IN HASKELL 
CSE 341

Input-output is problematic in a pure functional language.  For example,
remember that Haskell has referential transparency.  So if we have a
variable binding like this:

  s = "the input is: " ++ getLine

where getLine reads a line of input text, everywhere we have an occurence
of the variable s we ought to be able to substitute

"the input is: " ++ getLine 

But getLine needs to have a different value each time you use it!

There are several approaches to IO in pure functional languages, such as
streams and continuations.  Haskell now uses "monads", based on some
mathematically intense ideas from category theory.  In this set of lecture
notes we'll look at the pragmatics of doing IO in Haskell.

-}

module BasicInputOutput
    where

{- return a string consisting of a number and its sin and cos -}
trig_facts :: Float -> String
trig_facts x = "x=" ++ show x ++ " sin(x)=" ++ show (sin x) 
               ++ " cos(x)=" ++ show(cos x)

{- prompt for one number and print the trig_facts for it -}
print_one_trig_fact = do
  putStr "Please enter a number: " 
  s <- getLine
  let x = read s
  putStrLn (trig_facts x)

{- Note the expression 
  s <- getLine
(rather than s = getLine)

We write this with <- because 'getLine' is an action rather than an
ordinary function -- it can return different values.  This tells Haskell to
run the action getLine, and store the result (a string) in s.  But on the next
line, 'read' is an ordinary function, so we don't use the <- notation.
Instead, we need to use a let to bind the value of x. -}


{- simpler version using readLn -}
simpler_print_one_trig_fact = do
  putStr "Please enter a number: " 
  x <- readLn
  putStrLn (trig_facts x)

{- keep prompting for numbers and printing trig facts until the user says
   to stop -}
print_trig_facts = do
  putStr "Please enter a number: " 
  x <- readLn
  putStrLn (trig_facts x)
  putStr "again? "
  ans <- getLine
  if head ans == 'y' then print_trig_facts else return ()

{- () is the sole instance of the unit type ().  It's like null in Java. -}


{- writing a monadic function that returns a value -}
get_magic_number = do
  putStr "Please enter a magic number: " 
  n <- readLn
  if mod n 2 == 0 
    then do putStrLn "Sorry, even numbers are not magic ..."
            get_magic_number
    else return n

{- note that the value on the last line is 'return n' and not just 'n' --
   need this to get the type right -}

{- now we might like to write something like:
      10*get_magic_number
   but it won't work!  Wrong type!!  (Check this.)
   How can we get rid of the IO type?  Ans: you can't.
   Once some expression is 'infected' with being a monad, it
   infects anything of which it is a part. -}

ten_spot = do
  n <- get_magic_number
  return (10*n)