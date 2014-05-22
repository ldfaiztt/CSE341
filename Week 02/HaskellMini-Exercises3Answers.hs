{- Answers to Haskell Mini Exercises #3 -}

module Mini3
    where

import TypesNotes

-- inorder and postorder tree traversal functions

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

postorder :: Tree a -> [a]
postorder EmptyTree = []
postorder (Node x left right) = postorder left ++ postorder right ++ [x]




-- List is just like the built-in list type, but without such a nice syntax

data List a = Empty | Cell a (List a)
              deriving (Show,Read)

append Empty ys = ys
append (Cell x xs) ys = Cell x (append xs ys)

mymap f Empty = Empty
mymap f (Cell x xs) = Cell (f x) (mymap f xs)



-- some example lists
x = Cell 1 (Cell 2 (Cell 3 Empty))
y = Cell 10 (Cell 11 Empty)
z = append x y
m = mymap (*2) z
