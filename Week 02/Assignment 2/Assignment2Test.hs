{-
Chun-Wei Chen
CSE 341
10/09/12
-}

module Assignment2Test
	where
	
import Test.HUnit	
import Assignment2

{- This function is uesd to test whether a list of numbers is within epsilon of to 
another list's counterpart. -}
is_close [x] [y] = abs (x - y) < abs x  * epsilon
	where epsilon = 1.0e-6
is_close a b = abs (head a - head b) < abs (head a) * epsilon && is_close (tail a) (tail b)
	where epsilon = 1.0e-6

{- Test poly_multiply function. -}
poly_multiply_test1 = TestCase (assertEqual "mutiply two 0 polynomials" (poly_multiply [] []) [])
poly_multiply_test2 = TestCase (assertEqual "multiply a 0 polynomial with a non-zero polynomial" 
								(poly_multiply [Term (-3) 4, Term 1 1, Term 5 0] []) [])
poly_multiply_test3 = TestCase (assertEqual "multiply a non-zero polynomial with a 0 polynomial" 
								(poly_multiply [] [Term (-3) 4, Term 1 1, Term 5 0]) [])
poly_multiply_test4 = TestCase (assertEqual "multiply a polynomial with a constant" 
								(poly_multiply [Term 1 3, Term 1 1, Term (-1) 0] [Term (-5) 0])
								[Term (-5) 3, Term (-5) 1, Term 5 0])
poly_multiply_test5 = TestCase (assertEqual "test 5" 
								(poly_multiply [Term (-10) 2, Term 100 1, Term 5 0] 
								 [Term 1 999, Term (-1) 7, Term 1 1, Term 3 0]) 
								[Term (-10) 1001, Term 100 1000, Term 5 999, Term 10 9, Term (-100) 8, 
								 Term (-5) 7, Term (-10) 3, Term 70 2, Term 305 1, Term 15 0])
poly_multiply_tests = TestList [TestLabel "poly_multiply_test1" poly_multiply_test1,
								TestLabel "poly_multiply_test2" poly_multiply_test2,
								TestLabel "poly_multiply_test3" poly_multiply_test3,
								TestLabel "poly_multiply_test4" poly_multiply_test4,
								TestLabel "poly_multiply_test5" poly_multiply_test5]

{- Test show funciton. -}
show_test1 = TestCase (assertEqual "0 polynomial" (show (Poly "x" [])) "0")
show_test2 = TestCase (assertEqual "a constant" (show (Poly "x" [Term 100 0])) "100")
show_test3 = TestCase (assertEqual "2 terms polynomial" (show (Poly "s" [Term 4 3, Term (-5) 0]))
					   "4*s^3 - 5")
show_test4 = TestCase (assertEqual "4 terms polynomial" 
					   (show (Poly "x" [Term 4 3, Term (-3) 2, Term 2 1, Term (-1) 0])) 
					   "4*x^3 - 3*x^2 + 2*x - 1")
show_tests = TestList [TestLabel "show_test1" show_test1,
					   TestLabel "show_test2" show_test2,
					   TestLabel "show_test3" show_test3,
					   TestLabel "show_test4" show_test4]
{- Test golden_ratio function. -}
golden_ratio_test1 = TestCase (assertBool "first 1" (is_close [1.618034] (take 1 golden_ratio)))
golden_ratio_test2 = TestCase (assertBool "first 5" (is_close [1.618034,1.618034,1.618034,1.618034,1.618034]
													 (take 5 golden_ratio)))
golden_ratio_tests = TestList [TestLabel "golden_ratio_test1" golden_ratio_test1,
							   TestLabel "golden_ratio_test2" golden_ratio_test2]

{- Test sigma_square function. -}
sigma_square_test1 = TestCase (assertBool "sum square 1 to 4" (is_close [1,5,14,30] (take 4 sigma_square)))
sigma_square_test2 = TestCase (assertBool "sum square 1 to 10" (is_close [1,5,14,30,55,91,140,204,285,385] 
													            (take 10 sigma_square)))
sigma_square_tests = TestList [TestLabel "sigma_square_test1" sigma_square_test1,
							   TestLabel "sigma_square_test2" sigma_square_test2]

{- Combine all the tests into a list. -}
tests = TestList [TestLabel "poly_multiply_tests" poly_multiply_tests,
				  TestLabel "show_tests" show_tests,
				  TestLabel "golden_ratio_tests" golden_ratio_tests,
				  TestLabel "sigma_square_tests" sigma_square_tests]

{- Run all the tests. -}
run = runTestTT tests