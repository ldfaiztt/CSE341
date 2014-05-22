{- Assignment 1 Test
Chun-Wei Chen
CSE 341
09/30/12 

This is a test file for Assignment_1, which tests the functions in the Assignment_1.hs
to see if they work fine and return the correct outputs.
-}

module Assignment_1Test
	where

import Test.HUnit
import Data.Char	
import Assignment_1

-- This function is uesd to test whether a number is within epsilon of to another.
is_close x y = abs (x-y) < abs x * epsilon
	where epsilon = 1.0e-6

-- Test cone_volume function. test2 doesn't pass the test since it doesn't use is_close.
cone_volume_test1 = TestCase (assertBool "arithmetic test" (is_close (cone_volume 2 5) 20.943951))
cone_volume_test2 = TestCase (assertEqual "doesn't use is_close, bad test" (cone_volume 2 5) 20.943951)
cone_volume_tests = TestList [TestLabel "cone_volume test1" cone_volume_test1, 
							  TestLabel "cone_volume test2" cone_volume_test2]

-- Test squares function.							  
squares_test1 = TestCase (assertEqual "empty list" (squares []) [])
squares_test2 = TestCase (assertEqual "list with some integers" (squares [3,7,5,18]) [9,49,25,324])
squares_test3 = TestCase (assertEqual "infinite list" (take 5 (squares [2,4..])) [4,16,36,64,100])
squares_tests = TestList [TestLabel "squares test1" squares_test1,
						  TestLabel "squares test2" squares_test2,
						  TestLabel "squares test3" squares_test3]

-- Test map_squares function.						 
map_squares_test1 = TestCase (assertEqual "empty list" (squares []) [])
map_squares_test2 = TestCase (assertEqual "list with some integers" (squares [3,7,5,18]) [9,49,25,324])
map_squares_test3 = TestCase (assertEqual "infinite list" (take 5 (squares [2,4..])) [4,16,36,64,100])
map_squares_tests = TestList [TestLabel "map_squares test1" map_squares_test1,
							  TestLabel "map_squares test2" map_squares_test2,
							  TestLabel "map_squares test3" map_squares_test3]

-- Test pointfree_squares function.						  
pointfree_squares_test1 = TestCase (assertEqual "empty list" (squares []) [])
pointfree_squares_test2 = TestCase (assertEqual "list with some integers" (squares [3,7,5,18]) [9,49,25,324])
pointfree_squares_test3 = TestCase (assertEqual "infinite list" (take 5 (squares [2,4..])) [4,16,36,64,100])
pointfree_squares_tests = TestList [TestLabel "pointfree_squares test1" pointfree_squares_test1,
									TestLabel "pointfree_squares test2" pointfree_squares_test2,
									TestLabel "pointfree_squares test3" pointfree_squares_test3]

-- Test ascending funciton.									
ascending_test1 = TestCase (assertBool "empty list" (ascending []))
ascending_test2 = TestCase (assertBool "list with one integer" (ascending [1]))
ascending_test3 = TestCase (assertBool "list of several integers" (ascending [1,3,5,7,9]))
ascending_test4 = TestCase (assertBool "infinite list" (ascending (take 10 [1,2..])))
ascending_test5 = TestCase (assertBool "not in ascending order" (not (ascending [1,3,2])))
ascending_tests = TestList [TestLabel "ascending test1" ascending_test1,
							TestLabel "ascending test2" ascending_test2,
							TestLabel "ascending test3" ascending_test3,
							TestLabel "ascending test4" ascending_test4,
							TestLabel "ascending test5" ascending_test5]

-- Test parallel_resistors function. test5 doesn't pass the test since it doesn't use is_close.
parallel_resistors_test1 = TestCase (assertEqual "zero Ohm resistors" (parallel_resistors [0,0,0]) 0.0)
parallel_resistors_test2 = TestCase (assertBool "no resistor" 
                                     (isInfinite (pointfree_parallel_resistors [])))
parallel_resistors_test3 = TestCase (assertEqual "two same resistors" 
									 (parallel_resistors [10.0, 10.0]) 5.0)
parallel_resistors_test4 = TestCase (assertBool "two different resistors" 
									 (is_close (parallel_resistors [10.0, 20.0]) 6.666667))
parallel_resistors_test5 = TestCase (assertEqual "doesn't use is_close, bad test" 
									 (parallel_resistors [10.0, 20.0]) 6.666667)
parallel_resistors_tests = TestList [TestLabel "parallel_resistors test1" parallel_resistors_test1,
									 TestLabel "parallel_resistors test2" parallel_resistors_test2,
									 TestLabel "parallel_resistors test3" parallel_resistors_test3,
									 TestLabel "parallel_resistors test4" parallel_resistors_test4,
									 TestLabel "parallel_resistors test5" parallel_resistors_test5]

-- Test pointfree_parallel_resistors function, test5 doesn't pass the test since it doesn't use is_close. 									 
pointfree_parallel_resistors_test1 = TestCase (assertEqual "zero Ohm resistors" 
                                               (pointfree_parallel_resistors [0,0,0]) 0.0)
pointfree_parallel_resistors_test2 = TestCase (assertBool "no resistor" 
                                               (isInfinite (pointfree_parallel_resistors [])))
pointfree_parallel_resistors_test3 = TestCase (assertEqual "two same resistors" 
											   (pointfree_parallel_resistors [10.0, 10.0]) 5.0)
pointfree_parallel_resistors_test4 = TestCase (assertBool "two different resistors" 
									           (is_close (pointfree_parallel_resistors [10.0, 20.0]) 
											    6.666667))
pointfree_parallel_resistors_test5 = TestCase (assertEqual "doesn't use is_close, bad test" 
											   (pointfree_parallel_resistors [10.0, 20.0]) 6.666667)
pointfree_parallel_resistors_tests = TestList [TestLabel "pointfree_parallel_resistors test1" 
											   pointfree_parallel_resistors_test1,
											   TestLabel "pointfree_parallel_resistors test2" 
											   pointfree_parallel_resistors_test2,
											   TestLabel "pointfree_parallel_resistors test3" 
											   pointfree_parallel_resistors_test3,
											   TestLabel "pointfree_parallel_resistors test4" 
											   pointfree_parallel_resistors_test4,
											   TestLabel "pointfree_parallel_resistors test5" 
											   pointfree_parallel_resistors_test5]

-- Test palindrome function.											   
palindrome_test1 = TestCase (assertBool "banana palindrome" (palindrome "Yo! Banana Boy!"))
palindrome_test2 = TestCase (assertBool	"not palidrome" (not (palindrome "n!o*t-")))
palindrome_test3 = TestCase (assertBool "empty palindrome" (palindrome ""))
palindrome_test4 = TestCase (assertBool "date palindrome" (palindrome "11/02/2011"))
palindrome_tests = TestList [TestLabel "palindrome test1" palindrome_test1,
							 TestLabel "palindrome test2" palindrome_test2,
							 TestLabel "palindrome test3" palindrome_test3,
							 TestLabel "palindrome test4" palindrome_test4]

-- Combine all the tests into a list.							 
tests = TestList [TestLabel "cone_volume tests" cone_volume_tests,
				  TestLabel "squares tests" squares_tests,
				  TestLabel "map_squares tests" map_squares_tests,
				  TestLabel "pointfree_squares tests" pointfree_squares_tests,
				  TestLabel "ascending tests" ascending_tests,
				  TestLabel "parallel_resistors tests" parallel_resistors_tests,
				  TestLabel "pointfree_parallel_resistors tests" pointfree_parallel_resistors_tests,
				  TestLabel "palindrome tests" palindrome_tests]

-- Run all the tests.				  
run = runTestTT tests