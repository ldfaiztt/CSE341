{- 

Producing Executable Files with Haskell
CSE 341

To produce an executable file, you need to have a module called "Main" with 
a function called "main" with type IO ().

(The file can be called something other than Main.hs however.)

Then at the command line do this:

ghc --make Magic.hs -o magicprogram

'magicprogram' will now be an executable file.

For some reason, the Haskell compiler uses a slightly different I/O
library than the interpreter (at least on Linux and Mac).  As a
result, the compiled program has a nicer input behavior for readLn
that understands backspace, ^u, etc.

But on the output side, you need to have a newline at the end of the
end of the string being printed, or for the compiled version Haskell
won't output it until it has a complete line of text.

-}

module Main
    where

get_magic_number = do
  -- Unlike the interpreted version, you need to have a newline at the
  -- end of the end of the string being printed (see above).
  putStrLn "Please enter a magic number: " 
  n <- readLn
  if mod n 2 == 0 
    then do putStrLn "Sorry, even numbers are not magic ..."
            get_magic_number
    else return n

main = do
  n <- get_magic_number
  putStrLn ("The magic number is ... " ++ show n ++ "\n\n")