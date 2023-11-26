/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985,1986 by it.
 * 
 * All rights are reserved.
 */

% Nepolog Miscellaneous compatibility library.

:- initializing, lib quintus/compat.

?- int(X) when X.
int(X) :-
	integer(X).

?- gnot(G, _) when ground(G).
gnot(_, P) :-
	not call(P).

putatom(S, A) :-
	write(S, A).

putatom(A) :-
	write(A).
