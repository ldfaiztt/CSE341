{-
Chun-Wei Chen
CSE 341
10/09/12
-}

module Assignment2
	where

import qualified PolyParser

{- Define a Term type, which holds the coefficient and the exponentof the term. -}
data Term = Term Integer Integer
			deriving (Show,Eq)

{- Defines the ordering of polynomial's term by compare the exponent. -}
instance Ord Term where
	(Term a b) > (Term c d) = b > d
	(Term a b) >= (Term c d) = (b == d) && (a > c)
	(Term a b) <= (Term c d) = (b == d) && (a < c)
	(Term a b) < (Term c d) = b < d

{- Define a Terms type, which is a list of term and represents a polynomial. -}
type Terms = [Term]

{- This function multiplies two polynomials' terms. -}
multiply :: Term -> Term -> Term
multiply (Term a b) (Term c d) = Term (a * c) (b + d)

{- This function sorts the terms of a polynomial in by exponent (the largest exponent first). -}
sort :: Terms -> Terms
sort [] = []
sort [a] = [a]
sort (x:xs) = (sort (filter (> x) xs)) ++ (sort (filter (>= x) xs)) ++ 
			   [x] ++ (sort (filter (== x) xs)) ++ (sort (filter (<= x) xs)) ++ 
			   (sort (filter (< x) xs))

{- This function takes a polynomial and combines the terms with the same exponent. -} 
combine :: Terms -> Terms
combine [] = []
combine [a] = [a]
combine (a:b:c) = if not ((a > b) || (a < b))
					then combine ((plus a b) : c)					 
					else a : combine (b:c)

{- This function combines two terms with the same exponent. It's a helper function which helps 
combine function to combine the terms with the same exponent in a polynomial. -}				
plus :: Term -> Term -> Term
plus (Term x1 y1) (Term x2 y2) = Term (x1 + x2) y1

{- This function removes all the terms with coefficient of 0 in a polynomial. -}
remove_zero_coeff_terms :: Terms -> Terms
remove_zero_coeff_terms [] = []
remove_zero_coeff_terms (a:b) = if is_zero_coeff_terms a
								  then remove_zero_coeff_terms b
								  else a : remove_zero_coeff_terms b
									
{- This function determines whether the term is a term with coefficient of 0. It's a helper function 
which helps remove_zero_coeff_terms to drop all the terms with coefficient of 0. -}
is_zero_coeff_terms :: Term -> Bool
is_zero_coeff_terms (Term a _) = 
	if a == 0
	  then True
	  else False

{- This function multiplies two polynomials and returns the result. -}		
poly_multiply :: Terms -> Terms -> Terms
poly_multiply _ [] = []
poly_multiply [] _ = []
poly_multiply (x:xs) y = remove_zero_coeff_terms 
						 ((combine (sort (map (multiply x) y ++ poly_multiply xs y))))

{- Define a Polynomial type, which holds the name of the symbolic variable for a polynomial 
and its list of terms. -}
data Polynomial = Poly String Terms

{- This function returns a string representation of the polynomial. -}
instance Show Polynomial where
	show (Poly x []) = show 0
	show (Poly x [Term a b]) = if b == 0
								 then show a  -- constant
								 else if b == 1 
								   then if a == 1
										  then x  -- don't show the coefficient if it's 1
										  else show a ++ "*" ++ x
								   else if a == 1
									 then x ++ "^" ++ show b  -- don't show the coefficient if it's 1
									 else show a ++ "*" ++ x ++ "^" ++ show b
	show (Poly x ((Term a b):(Term c d):e)) = if c < 0
											    then show (Poly x [Term a b]) ++ " - " ++ 
												     show (Poly x ((Term (c*(-1)) d):e))
											    else show (Poly x [Term a b]) ++ " + " ++ 
												     show (Poly x ((Term c d):e))

{- This is an interactive function which asks user input two polynomials and 
calculates the result of multiplying them. -}
poly_calc :: IO ()													 
poly_calc = do 
	putStr "Please enter a polynomial: "
	s <- getLine
	let x1 = fst (PolyParser.string_to_raw_poly s)  -- variable of the first polynomial 
	let x2 = snd (PolyParser.string_to_raw_poly s)  -- terms of the first polynomial
	if s == ""
	  then return()  -- end prompting if the user enter a blank line
		else do
		  putStr "Please enter another polynomial: "
		  s <- getLine
		  let y1 = fst (PolyParser.string_to_raw_poly s)  -- variable of the second polynomial
		  let y2 = snd (PolyParser.string_to_raw_poly s)  -- terms of the first polynomial
		  if x1 /= y1  -- check if two entered polynomials' variables are the same
			then if x1 == ""
			       -- if first variable is empty (constant) but second one is not, use the second one
				   then putStrLn ("(" ++ show (Poly y1 (to_sorted_terms x2)) ++ ") * (" ++
						          show (Poly y1 (to_sorted_terms y2)) ++ ") => " ++ 
						          show (Poly y1 (poly_multiply (to_sorted_terms x2) (to_sorted_terms y2))))
					 else if y1 == ""
					   -- if second variable is empty (constant) but first one is not, use the first one
					   then putStrLn ("(" ++ show (Poly x1 (to_sorted_terms x2)) ++ ") * (" ++
							          show (Poly x1 (to_sorted_terms y2)) ++ ") => " ++ 
							          show (Poly x1 (poly_multiply (to_sorted_terms x2) 
									                 (to_sorted_terms y2))))
					   -- show the message to user when trying to use two different variables, 
					   -- and ask for new pairs of polynomials
					   else putStrLn "Please enter two polynomials with same variable." >> poly_calc
			      else
				  putStrLn ("(" ++ show (Poly x1 (to_sorted_terms x2)) ++ ") * (" ++
						    show (Poly y1 (to_sorted_terms y2)) ++ ") => " ++ 
						    show (Poly x1 (poly_multiply (to_sorted_terms x2) (to_sorted_terms y2))))
		  poly_calc  -- prompt for more polynomials

{- This function converts the [(Integer,Integer)] part in the string representation of a polynomial 
(the outputstring_to_raw_poly function) into Terms. -} 	
to_sorted_terms :: [(Integer, Integer)] -> Terms 
to_sorted_terms [] = []
to_sorted_terms [(a,b)] = [Term a b]
to_sorted_terms ((a,b):c) = sort (Term a b : (to_sorted_terms c))

{- This function produces a list of approximation of golden ratio. -} 
golden_ratio = map (/2) (map (+1) (approx_sqrt_list 5))

{- This function produces a list of approximations of the square root of the passed in number. -}
approx_sqrt_list x = filter close_enough (approximations x)
					 where approximations a = a : approximations ((a + x / a) / 2)
					       close_enough r = abs (x - r * r) < 1.0e-6

{- This function produces a list of sum of integers' square by using the sigma formula. 
I do another one for infinite data structure since I just modify the sqt in InfiniteDataStructures
to make it become golden ratio function. -}
sigma_square = [n^3 / 3 + n^2 / 2 + n / 6 | n <- [1..]]