/* CSE 341 - clpr examples */

/* As we've discussed, arithmetic in standard Prolog is not as elegant as 
other constructs for things like list and tree manipulation.  SWI Prolog
includes a variety of constraint libraries that add a constraint capability
to the language.  The result are capabilities much more in line with the
Prolog orientation toward logic and declarative programming.  See 
Appendix A.8 of the SWI Prolog for more information.

Here we'll look at the clpr library for constraints on
real numbers.  This has to be loaded explicitly, as follows: */

:- use_module(library(clpr)).

/* Here are some of the  arithmetic examples from the Prolog Basics 
lecture notes, redone using constraints.  Note that constraints are
written in {curly brackets}; the variables in the constraints can be
freely used in ordinary Prolog rules as well. 

Try these with various combinations of constants and variables */

/* centigrade to fahrenheit converter */
/* old definition: fahrenheit(C,F) :- F is 1.8*C+32.0.  */
cf(C,F) :- {F = 1.8*C+32.0}.

myabs(X,X) :- {X>=0}.
myabs(X,X1) :- {X<0, X1 = -X}.

mymax(X,Y,X) :- {X>=Y}.
mymax(X,Y,Y) :- {X<Y}.

quicksort([],[]).
quicksort([X|Xs],Sorted) :-
  partition(X,Xs,Smalls,Bigs),
  quicksort(Smalls,SortedSmalls),
  quicksort(Bigs,SortedBigs),
  append(SortedSmalls,[X|SortedBigs],Sorted).

partition(_,[],[],[]).
partition(Pivot,[X|Xs],[X|Ys],Zs) :-
   {X =< Pivot},  /* note that this is a constraint */
   partition(Pivot,Xs,Ys,Zs).
partition(Pivot,[X|Xs],Ys,[X|Zs]) :-
   {X > Pivot},   /* the other constraint */
   partition(Pivot,Xs,Ys,Zs).

/* the double relation */
double(X,Y) :- {Y=2*X}.


/* Solve for the currents (or other parameters) for a network consisting
of a battery and three resistors in parallel.  The variables are as 
follows:
   V is the voltage of the battery
   I0 is the current flowing out of the positive lead of the battery
   I1, I2, I3 are the currents flowing into the three resistors
   R1, R2, R3 are the resistances of the three resistors

The first constraint represents Kirchhoff's Law for the top node connecting
the positive lead from the battery to each resistor.  (Be careful to keep
the directions of the currents straight.)  The other constraints represent
Ohm's Law for each resistor. */
   
parallel_resistors(V,I0,R1,R2,R3) :-
   {I0 = I1+I2+I3},
   {V = I1*R1},
   {V = I2*R2},
   {V = I3*R3}.

/* sample goal - find the current given the voltage and resistances */
go1(I) :- parallel_resistors(10,I,10,20,30).

/* find the relation between voltage and current */
go2(V,I) :- parallel_resistors(V,I,30,30,30).

/* find an unknown resistance */
go3(R3) :- parallel_resistors(10,1,100,200,R3).

/* The clpr library can solve arbitrary linear equality and inequality
constraints.  Try some simultaneous equations, and also inequality
constraints that are unsatisfiable (even though we never give enough
information to find a specific value for the constrained variable).
Examples:

{X+Y=10, X-Y=2}.
{2*X + 3*Y + Z = 21, X + Y - Z = 1, 10*X + Y + 1 = Z + 20}.
{X+2*Y = 10}.
{X > 5, X < 3}.


Nonlinear constraints (quadratics, constraints involving exponentiation, 
sin, cos, tan, etc) are delayed until there is enough information to
solve them directly.  Examples:

{1.0 = sin(X)}.  -- solved immediately
{A = sin(X)^2 + cos(X)^2}.  -- delayed

{A = sin(X)^2 +cos(X)^2, X=0.5}.  -- solved

*/




