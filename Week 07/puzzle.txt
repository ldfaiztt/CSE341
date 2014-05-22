/* CSE 341 - brief tour of the finite domain constraint solver.
Finite domain constraints can be used to model and solve various
combinatorial problems. */

:- use_module(library(clpfd)).

/* A few sample goals:

X#>3.
X#>3, X#<10.

X#>3, X#<10, Y#<10, Y#=X+5.

Vs = [X,Y,Z], Vs ins 1..3, all_different(Vs), X = 1, Y #\= 2.

We can use 'label' to systematically try out values for a list of
finite domain variables until all of them are ground:

X#>3, X#<10,  Y#=X+5, label([Y]).


*/


puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
        Vars = [S,E,N,D,M,O,R,Y],
        Vars ins 0..9,
        all_different(Vars),
                  S*1000 + E*100 + N*10 + D +
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
        M #\= 0, S #\= 0.

/* Sample queries for puzzle.

This query won't solve for all the variables:
?- puzzle(As+Bs=Cs). 

?- puzzle(As+Bs=Cs), label(As).

Or we can try just labelling one variable:
?- puzzle(As+Bs=Cs), As=[S,E,N,D], label([N]).

?- puzzle(As+Bs=Cs), Bs=[M,O,R,E], label([E]).

In nearly all cases this gives a unique solution.
*/
