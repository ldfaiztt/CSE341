{- CSE 341, Haskell.  

Simple example of using modules.  Here we declare a module called
AquariumTest, which imports another module called Aquarium.

Haskell looks for this in a file named Aquarium.hs (in this case, in the
same directory).

After the import we can reference the declarations in Aquarium either as
qualified names or directly.

There are additional capabilities for specifying exactly what gets exported
from a module (which we won't go into in this class due to lack of time).
-}

module AquariumTest
    where

import Aquarium

result1 = Aquarium.octopus + 8
result2 = octopus + 12

{- If we did 'import qualified Aquarium' instead we could only reference
   octopus using the qualified name. -}