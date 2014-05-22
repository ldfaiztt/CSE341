/* CSE 341 - Prolog code to accompany basics.html notes */

/* the 'likes' example */
likes(fred,beer).
likes(fred,cheap_cigars).
likes(fred,monday_night_football).

likes(sue,jogging).     
likes(sue,yogurt).     
likes(sue,bicycling).     
likes(sue,amy_goodman).

likes(mary,jogging).     
likes(mary,yogurt).     
likes(mary,bicycling).     
likes(mary,rush_limbaugh).

health_freak(X) :-
   likes(X,yogurt),
   likes(X,jogging).

left_wing(X) :-
   likes(X,amy_goodman).

right_wing(X) :-
   likes(X,rush_limbaugh).

low_life(X) :-
  likes(X,cheap_cigars).


/******************************************************/

/* Some CSE majors courses and their prerequisites.  This simplifies
   the actual CSE curriculum by assuming courses have at most one
   direct prerequisite. */

prerequisite(cse142,cse143).

prerequisite(cse143,cse311).

prerequisite(cse311,cse312).

prerequisite(cse143,cse331).

prerequisite(cse143,cse341).

/* take_before(A,B) succeeds if you must take A before B */
take_before(X,Z) :- prerequisite(X,Z).
take_before(X,Z) :- prerequisite(X,Y),
                    take_before(Y,Z).

/******************************************************/

/* Some list manipulation programs */

myappend([],Ys,Ys).
myappend([X|Xs],Ys,[X|Zs]) :- myappend(Xs,Ys,Zs).

mymember(X,[X|_]).
mymember(X,[_|Ys]) :- mymember(X,Ys).

/* Some simple arithmetic examples in CLP(R) */

/* centigrade to fahrenheit converter */
fahrenheit(C,F) :- F is 1.8*C+32.0.

myabs(X,X) :- X>=0.
myabs(X,X1) :- X<0, X1 is -X.

mymax(X,Y,X) :- X>=Y.
mymax(X,Y,Y) :- X<Y.

/* length of a list */
mylength([],0).
mylength([_|Xs],N1) :- mylength(Xs,N), N1 is N+1.

factorial(0,1). 
factorial(N,F) :-  
   N>0, 
   N1 is N-1, 
   factorial(N1,F1), 
   F is N * F1.

/* more list manipulation examples */

/* find all permutations of a list */
permute([],[]).
permute([H|T],L) :-
  permute(T,U),
  insert(H,U,L).

/* insert an element X somewhere in list L */

insert(X,L,[X|L]).
insert(X,[H|T],[H|U]) :-
  insert(X,T,U).

/* inefficient sort */

badsort(L,S) :- permute(L,S), sorted(S).

sorted([]).
sorted([_]).
sorted([A,B|R]) :-
  A=<B,
  sorted([B|R]).

/* a better sort (quicksort) */

quicksort([],[]).
quicksort([X|Xs],Sorted) :-
  partition(X,Xs,Smalls,Bigs),
  quicksort(Smalls,SortedSmalls),
  quicksort(Bigs,SortedBigs),
  myappend(SortedSmalls,[X|SortedBigs],Sorted).

partition(_,[],[],[]).
partition(Pivot,[X|Xs],[X|Ys],Zs) :-
   X =< Pivot,
   partition(Pivot,Xs,Ys,Zs).
partition(Pivot,[X|Xs],Ys,[X|Zs]) :-
   X > Pivot,
   partition(Pivot,Xs,Ys,Zs).


