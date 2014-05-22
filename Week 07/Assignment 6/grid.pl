/* CSE 341 - starter program for HW 6, grid problem.
  Find the temperatures at each point on a grid (for example, a metal plate).
The grid is represented as a list of lists of numbers.  Each interior point
on the grid is constrained to be the average of its 4 neighbors.  Taken
from Kim Marriott and Peter Stuckey, "Programming with Constraints: An
Introduction", who in turn took it from the CLP(R) example programs from
Monash University.  Adapted to use SWI Prolog.
*/

:- use_module(library(clpr)).

rows([_,_]).
rows([R1,R2,R3|Rs]):- cols(R1,R2,R3), rows([R2,R3|Rs]).

cols([_,_],[_,_],[_,_]).
cols([TL,T,TR|Ts],[ML,M,MR|Ms],[BL,B,BR|Bs]):-
  /* constrain the midpoint to be the average of its neighbors */
  {M = (T + ML + MR + B) / 4},
  cols([T,TR|Ts], [M,MR|Ms], [B,BR|Bs]).


/* rule to print out a matrix in readable form */

print_matrix([H|T]):-
   print_vector(H),
   print_matrix(T).
print_matrix([]).

print_vector([H|T]):-
   format('~|~t~2f~7+', H),
   print_vector(T).
print_vector([]):-
   format('~n',[]).

/* sample goal */

grid1:-
   X =  [
    [0,0,0,0,0,0,0,0,0,0,0],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,_,_,_,_,_,_,_,_,_,100],
    [100,100,100,100,100,100,100,100,100,100,100]
    ],
   rows(X),
   format('~nTemperatures: ~n',[]),
   print_matrix(X).

% Answer:
%    0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00   0.00
%  100.00  51.11  32.52  24.56  21.11  20.12  21.11  24.56  32.52  51.11 100.00
%  100.00  71.91  54.41  44.63  39.74  38.26  39.74  44.63  54.41  71.91 100.00
%  100.00  82.12  68.59  59.80  54.97  53.44  54.97  59.80  68.59  82.12 100.00
%  100.00  87.97  78.03  71.00  66.90  65.56  66.90  71.00  78.03  87.97 100.00
%  100.00  91.71  84.58  79.28  76.07  75.00  76.07  79.28  84.58  91.71 100.00
%  100.00  94.30  89.29  85.47  83.10  82.30  83.10  85.47  89.29  94.30 100.00
%  100.00  96.20  92.82  90.20  88.56  88.00  88.56  90.20  92.82  96.20 100.00
%  100.00  97.67  95.59  93.96  92.93  92.58  92.93  93.96  95.59  97.67 100.00
%  100.00  98.89  97.90  97.12  96.63  96.46  96.63  97.12  97.90  98.89 100.00
%  100.00 100.00 100.00 100.00 100.00 100.00 100.00 100.00 100.00 100.00 100.00

