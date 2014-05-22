-- Solutions to CSE 341 Haskell Discussion Questions

-- Write a Haskell function to find the cube of a  number.  
cube :: Double -> Double
cube n = n^3

-- Write a Haskell function to find the sum of three Doubles
sum3 :: Double -> Double -> Double -> Double
sum3 x y z = x+y+z


-- Write a Haskell function to find the value of the quadratic
-- expression ax^2 + bx + c for parameters a, b, c, and x.   
quad_expr a b c x = a*x^2 + b*x + c


-- Write a Haskell function to reverse a list.  
my_rev :: [a] -> [a]
my_rev [] = []
my_rev (x:xs) = my_rev xs ++ [x]

-- here's a more efficient version that uses a helper function revappend
my_rev2 :: [a] -> [a]
my_rev2 s = revappend s []

-- reverse the first list, appending the second to the result
revappend :: [a] -> [a] -> [a]
revappend [] rest = rest
revappend (x:xs) rest = revappend xs (x:rest)

-- Write a function my_map2 that is analogous to map but works
-- for functions of two arguments rather than one.  
my_map2 :: (a->b->c) -> [a] -> [b] -> [c]
my_map2 f [] [] = []
my_map2 f (x:xs) (y:ys) = f x y : my_map2 f xs ys

-- Give a recursive definition of a variable doubles whose first element is
-- 10, and whose n-th element is twice the n-1 st, i.e
--     [10, 20, 40, 80, 160, 320, ....]
doubles :: [Integer]
doubles = 10 : map (*2) doubles

-- using a helper function doubles_from that returns a list of all the
-- doubles starting at n:

doubles2 = doubles_from 10
doubles_from n = n : doubles_from (2*n)


-- using iterate:
doubles3 = iterate (*2) 10


-- Write a Haskell function to return the infinite list of amounts of
-- money you have every year, assuming you start with $100 and get paid 5%
-- interest, compounded yearly.

-- simple but not general version:
dollars :: [Double]
dollars = 100 : map (\d -> 1.05*d) dollars

-- or using iterate:
idollars = iterate (1.05*) 100

-- more general recursive version:
better_dollars :: [Double]
better_dollars = dollar_growth 100.0 0.05

dollar_growth :: Double -> Double -> [Double]
dollar_growth p rate = p : dollar_growth (p*(1+rate)) rate


-- Suppose that the following Haskell script has been filed in ...

my_const c x = c

append [] ys = ys
append (x:xs) ys = x : append xs ys

my_map f [] = []
my_map f (x:xs) = f x : my_map f xs

-- What is the result of evaluating the following Haskell expressions?  
-- Try them and see ...