/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- utilities.

wait _.

co$max(A, B, A) :- A >= B.
co$max(A, B, B) :- A < B.

?- co$max(_, _.X) when X.
co$max(X, [X]).
co$max(Max, X.XT) :-
	XT = _._,
	co$max(Y, XT),
	co$max(X, Y, Max).

%co$min(A, B, B) :- A >= B.
%co$min(A, B, A) :- A < B.
%
%?- co$min(_, _.X) when X.
%co$min(X, [X]).
%co$min(Min, X.XT) :-
%	XT = _._,
%	co$min(Y, XT),
%	co$min(X, Y, Min).

iotaL(M, N, []) :-
	M > N.
iotaL(M, N, M.Iota) :-
	M =< N,
	M1 is M + 1,
	iotaL(M1, N, Iota).

putIdentifier(Ident) :-
	( Ident == [] ->
		putl("[]")
	;	atom(Ident),
		writev(2'01100001010, Ident)
	).

%	Keys must be unique.
makeKeyedLookupTree(List, Tree) :-
	length(List, N),
	keySort(List, Sorted),
	makeBalancedTree(N, Sorted, Tree).

%	Non-logical.
%	Only finds one occurance of Key.
?- lookupKeyedTree(_, X, _) when X.
lookupKeyedTree(Key, leaf(Key, Value), Value).
lookupKeyedTree(Key, tree(Left, K, Right), Value) :-
	( Key @< K ->
		lookupKeyedTree(Key, Left, Value)
	;	lookupKeyedTree(Key, Right, Value)
	).

?- makeBalancedTree(N, L, T) when N or L or T.
makeBalancedTree(0, [], void).
makeBalancedTree(1, [Key - Value], leaf(Key, Value)).
makeBalancedTree(N, List, tree(Left, Key, Right)) :-
	N > 1,
	LN is N // 2,
	RN is N - LN,
	splitList(LN, List, LeftList, RightList),
	RightList = (Key - _)._,
	makeBalancedTree(LN, LeftList, Left),
	makeBalancedTree(RN, RightList, Right).

%	Could be defined as
%		splitList(N, X, Y, Z) :- length(X, N), append(X, Y, Z).
?- splitList(N, X, Y, _) when ground(N) or X or Y.
splitList(0, X, [], X).
splitList(N, X.XT, X.YT, Z) :-
	N > 0,
	N1 is N - 1,
	splitList(N1, XT, YT, Z).
