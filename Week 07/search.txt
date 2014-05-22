/* examples to illustrate controlling search in Prolog */

/* append with the rules reversed */

myappend([X|Xs],Ys,[X|Zs]) :- myappend(Xs,Ys,Zs).
myappend([],Ys,Ys).

/* the standard definition of member */
mymember(X,[X|_]).
mymember(X,[_|Ys]) :- mymember(X,Ys).

/* cut (written !) prunes the search tree */

member_cut(X,[X|_]) :- !.
member_cut(X,[_|Ys]) :- member_cut(X,Ys).

/*

mymember(1,[1,2,3,1])   will succeed twice

member_cut(1,[1,2,3,1])   will succeed just once

member_cut(X,[1,2,3]) will only give one answer: X=1.

*/

not(X) :- call(X), !, fail.
not(_).

/* there is a built-in version of not, written as \+ */

/* Advanced topic - example for discussion of details of cut */

creatures([squid,octopus]).
creatures([dolphin,whale,porpoise]).

/* try these goals:
creatures(L), member(C,L).
creatures(L), member_cut(C,L).
*/
