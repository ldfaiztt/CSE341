/* CSE 341 - DIFFERENCE LISTS */

/* Difference lists are an elegant programming technique in Prolog.
A difference list A\B represents the list resulting from snipping
B off of the end of A.  For example these are all difference list
representations of [1,2,3]:

[1,2,3]\[]
[1,2,3,100,200]\[100,200]
[1,2,3|T]\T

Further, if you represent the list initially as [1,2,3|T]\T, subsequent
derivation steps can further constrain T -- without changing the list that
the difference list represents!

In effect, this lets you append a list onto another partially constructed
list, while still maintaining a clean logical semantics.  In contrast, in
Scheme/Racket or another imperative language you'd do list surgery for this.
*/


/* declare "\" as a new infix operator (with higher precedence than = )*/
:- op(610,xfx,\).


/* trivial version of append using difference lists */

dl_append(X\XT, Y\YT, Z\ZT) :- Z=X, XT=Y, YT=ZT.

/* or more compactly: */

dl_append_short(X\Y, Y\YT, X\YT).

/* try this: dl_append([1,2,4|T]\T, [10,11,12,13,14|U]\U, Z\[]).  */

/* standard version of append */

myappend([],Ys,Ys).
myappend([X|Xs],Ys,[X|Zs]) :- myappend(Xs,Ys,Zs).


/* Naive rule for reversing a list.  This is simple but ends up 
   repeatedly copying the list as it's reversed. */
naive_reverse([],[]).
naive_reverse([X|Xs],Ys) :- naive_reverse(Xs,Rs), myappend(Rs,[X],Ys).

/* Difference list version of reverse */
reverse(Xs,Rs) :- reverse_dl(Xs,Rs\[]).

reverse_dl([],T\T).
reverse_dl([X|Xs],Rs\T) :- reverse_dl(Xs,Rs\[X|T]).


/* Naive flatten: (note that this will only work correctly with a ground list
   as the first argument, and you can't backtrack -- otherwise you need to use cut) */

flatten([],[]).

flatten([X|Xs],Y) :-
   flatten(X,XF),
   flatten(Xs,XsF),
   myappend(XF,XsF,Y).

flatten(X,[X]).


/* Flatten using difference lists: */
dflatten(S,F) :-
  flatten_dl(S, F\[]).


flatten_dl([], X\X).

flatten_dl([X|Xs], Y\Z) :-
   flatten_dl(X, Y\T),
   flatten_dl(Xs, T\Z).

flatten_dl(X, [X|Z]\Z).


/* standard version of quicksort */

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


/* difference list version of quicksort */

quicksort2(L,S) :- quicksort_dl(L,S\[]).

quicksort_dl([],S\S).

quicksort_dl([X|Xs],Sorted\Tail) :-
  partition(X,Xs,Smalls,Bigs),
 /* the recursive calls splice the two lists together, with the split element
    in the middle */
  quicksort_dl(Smalls,Sorted\[X|T]),  
  quicksort_dl(Bigs,T\Tail).

/* partition is the same as above */

/* Finally, you can have difference trees and other data structures, not just lists. */
