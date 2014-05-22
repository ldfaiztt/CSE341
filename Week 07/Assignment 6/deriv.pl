/* CSE 341 - starter code for deriv program */

/* Find the derivative of expression A with respect to variable X.
   Use cut to prune away any alternate solutions. */
deriv(A,X,C) :- basic_deriv(A,X,B), simplify(B,C), !.

basic_deriv(N,_,0) :- number(N).
basic_deriv(X,X,1).
basic_deriv(Y,X,0) :- atom(Y), Y\==X.

basic_deriv(A+B,X,A1+B1) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

basic_deriv(A*B,X,A*B1+A1*B) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

simplify(X,X) :- atom(X).
simplify(N,N) :- number(N).

simplify(A+B,C) :- 
   simplify(A,A1),
   simplify(B,B1),
   simplify_sum(A1+B1,C).

simplify(A*B,C) :- 
   simplify(A,A1),
   simplify(B,B1),
   simplify_product(A1*B1,C).

/* put additional simplify rules here */

simplify_sum(0+A,A).
simplify_sum(A+0,A).
simplify_sum(A+B,C) :- number(A), number(B), C is A+B.
simplify_sum(A+B,A+B).

simplify_product(0*_,0).
simplify_product(_*0,0).
simplify_product(1*A,A).
simplify_product(A*1,A).
simplify_product(A*B,C) :- number(A), number(B), C is A*B.
simplify_product(A*B,A*B).

:- begin_tests(deriv).

test(const) :- deriv(3,x,0).
test(x) :- deriv(x,x,1).
test(y) :- deriv(y,x,0).
test(plus) :- deriv(x+3,x,1).
test(plus) :- deriv(x+y,x,1).
test(plus_unsimp) :- deriv((2+3)*x,x,5).
test(times) :- deriv( (x+1)*(x-1), x, (x+1)+(x-1)).
test(times) :- deriv( (x*y)*(x+3), x, (x*y)+(y*(x+3))).

:- end_tests(deriv).

:-    run_tests.
