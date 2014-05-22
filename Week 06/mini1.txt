/* CSE 341 - Mini Exercises #1 - Selected Answers */

/* Questions 1, 2, 6 .... try them! */

/* Question 2 -- write a Prolog rule twins that succeeds if the second
   argument is a list containing all the elements of the first list,
   repeated. */

twins([],[]).
twins([X|Xs],[X,X|Ys]) :- twins(Xs,Ys).


/* Write a Prolog rule to reverse a list */
append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

reverse([],[]).
reverse([X|Xs],Ys) :- reverse(Xs,Rs), append(Rs,[X],Ys).

/* more efficient reverse: */
reverse2(Xs,Rs) :- revappend(Xs,[],Rs).
revappend([],Ys,Ys).
revappend([X|Xs],Ys,Zs) :- revappend(Xs,[X|Ys],Zs).


sum([],0).
sum([X|Xs],S) :- sum(Xs,S1), S is S1+X.

