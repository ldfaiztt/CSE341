/* CSE 341 - Unit testing in SWI Prolog 
   Here are some simple examples of doing unit tests.  For additional 
   details see http://www.swi-prolog.org/pldoc/package/plunit.html 
   To use, just read in this unit testing file. */

/* We'll use the basics lecture notes for examples.  Note the syntax
   here for a directive for consulting another file. */
:- consult(basics).

:- begin_tests(basics).

/* a test case for a rule with a single answer */
test(temperature) :- fahrenheit(100.0,212.0).

/* another test case with a single answer */
test(permute) :- permute([],[]).

/* test case where there are multiple answers (just check for one answer) */
test(permute, [nondet]) :- permute([a,b,c],[b,c,a]).

/* testing for failure */
test(fred, [fail]) :- likes(fred,yogurt).

/* check for all answers from a nondeterministic predicate */
test(fred2, all(X == [beer,cheap_cigars,monday_night_football])) :-
   likes(fred,X).

/* checking for all answers from another nondeterministic predicate */
test(allpermute, all(X == [[a, b, c], [b, a, c], [b, c, a], [a, c, b], [c, a, b], [c, b, a]])) :-
   permute([a,b,c],X).

/* A test case for a rule with a single answer, but where Prolog thinks 
   there is still a choice point after the goal succeeds.  We can avoid 
   Prolog grumbling about 'Test succeeded with choicepoint' by using
   the 'all' construct here, with the single answer. */
test(factorial, all(X==[24])) :- factorial(4,X).

:- end_tests(basics).

/* The following directive runs the tests. You can also give the goal
   run_tests on the command line (without the :- part). */
:-    run_tests.
