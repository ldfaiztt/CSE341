/* Chun-Wei Chen
   CSE 341
   Assignment 5
   11/05/12 */

/* repeat succeeds if the second argument is a list with 0 or more occurences of the first argument. */   
repeat(_, []).
repeat(X, [X|Xs]) :- repeat(X, Xs).

/* Compute the average of a list of numbers. */
average([X|Xs], Y) :- sum([X|Xs], S), length([X|Xs], N), Y is S / N.

/* Compute the sum of a list of numbers. */
sum([], 0).
sum([X|Xs], Y) :- sum(Xs, Z), Y is Z + X.

/* Facts about noun. */
noun(human).
noun(dog).
noun(cat).
noun(bull).

/* Facts about verbs. */
verb(swims).
verb(runs).
verb(eats).
verb(drinks).

/* Sentence is consist of a "the", a noun, and a verb. I found 16 different answers when I tried my goal 
   with variables for all three arguments. There is only one possibility for the first argument of sentence.
   When sentence uses human as the second argument, there are only four possibilies for verb, the third argument.
   And the other three nouns also have four possibilites for verb. So there are 16 different answers since
   1 * 4 * 4 = 16. */
sentence(the, N, V) :- noun(N), verb(V).

/*******************************************************/

/* Unit tests */
:- begin_tests(assignment5_prolog).

test(repeat1, [nondet]) :- repeat(cat, []).
test(repeat2, [nondet]) :- repeat(dog, [dog, dog, dog]).
test(repeat3, [fail]) :- repeat(cat, [cat, cat, cat, dog]).

test(average1, [fail]) :- average([], 0).
test(average2) :- average([3], 3).
test(average3) :- average([1, 2, 3], 2).

test(sentence1) :- sentence(the, human, swims).
test(sentence2) :- sentence(the, bull, runs).
test(sentence3) :- sentence(the, cat, drinks).
test(sentence4) :- sentence(the, dog, eats).
test(sentence5, [fail]) :- sentence(dog, cat, swims).
test(sentence6, (X == the)) :- sentence(X, human, eats).
test(sentence7, all(X == [human, dog, cat, bull])) :- sentence(the, X, swims).
test(sentence8, all(X == [swims, runs, eats, drinks])) :- sentence(the, cat, X).

:- end_tests(assignment5_prolog).

/* Run the tests. */
:- run_tests.