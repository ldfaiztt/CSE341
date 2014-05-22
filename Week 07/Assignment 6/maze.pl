/* CSE 341 - starter code - path through a maze */


edge(allen_basement, atrium, 5).
edge(atrium, hub, 10).
edge(hub, odegaard, 140).
edge(hub, red_square, 130).
edge(red_square, odegaard, 20).
edge(red_square, ave, 50).
edge(odegaard, ave, 45).
edge(allen_basement, ave, 20).

/* define the path rule here */

:- begin_tests(maze).

/* Three different tests for a path from allen_basement to the ave */

/* First a nondeterministic test of one path: */
test(maze, [nondet]) :- path(allen_basement, ave, 
     [allen_basement, atrium, hub, odegaard, ave], 200).

/* A test for all of the costs for the 4 possible paths (ignoring the route): */
test(maze, all(C==[20, 200, 195, 210])) :- path(allen_basement, ave, _, C).

/* Finally an exhaustive test of all 4 paths, including both route and costs.
  To make this work with Prolog's unit test framework, we bundle the route
  and cost together into a 'solution', and have a list of solutions.  (The
  name 'solution' is arbitrary -- this could be any name and would still work.) */
test(maze, all(Soln==[ 
   solution([allen_basement, ave],20),
   solution([allen_basement, atrium, hub, odegaard, ave],200),
   solution([allen_basement, atrium, hub, red_square, ave],195),
   solution([allen_basement, atrium, hub, red_square, odegaard, ave],210) 
   ])) :-
   path(allen_basement, ave, S, C), Soln = solution(S,C).


:- end_tests(maze).

:- run_tests.
