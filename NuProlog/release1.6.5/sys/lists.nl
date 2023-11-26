/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

% Nepolog list manipulation predicates.

/*	_3 is _2 appended to _1 */
:- append(A, _, C) when A or C.
append([], A, A).
append(A.B, C, A.D) :- append(B, C, D).

/*	_3 is _2 with element _1 removed */
:- delete(_, B, C) when B or C.
delete(A, A.B, B).
delete(A, B.C, B.D) :- delete(A, C, D).

:- length(A, B) when A or B.
length([], 0).
length(_.L, N) :-
	N > 0,
	plus(N1, 1, N),
	length(L, N1).

/*	_1 is a member of _2 */
:- member(_, B) when B.
member(A, A._).
member(A, _.C) :- member(A, C).

:- memberchk(_, B) when B.
memberchk(A, A._) :- !.
memberchk(A, _.C) :- member(A, C).

:- notMember(_, Y) when Y.
notMember(_, Y) :-
	Y ~= _._.
notMember(X, Y.YT) :-
	X ~= Y,
	notMember(X, YT).

:- member(_, B, _) when B.
member(A, B, B) :-
	B = A._.
member(A, _.B, C) :- member(A, B, C).

%	Unimplemented pending administrative decision.
%/*	_2 contains exactly _3 occurrences of _1 */
%:- nmember(_, B, C) when B /*or C*/.
%nmember(A, [], 0).
%nmember(A, A.C, N) :- nmember(A, C, N1), N is N1 + 1.
%nmember(A, B.C, N) :- A ~= B, nmember(A, C, N).

:- $last(_.X, _) when X.
$last([X], X).
$last(_.X, Y) :-
	$last(X, Y).

:- suffix(X, _) when X.
suffix(X, X).
suffix(_.X, Y) :-
	suffix(X, Y).

:- isList(X) when X.
isList([]).
isList(_.L) :-
	isList(L).

$list(X) :-
	var(X),
	!, fail.
$list([]).
$list(_.L) :-
	$list(L).

/*	_2 is a permutation of _1 */
perm(A, B) :-
	$perm(A, B, B).

:- $perm(A, _, B) when A or B.
$perm([], [], []).
$perm(A.AT, C, _.DT) :- $perm(AT, CT, DT), delete(A, C, CT).

% Note that
%	perm([], []).
%	perm(A.AT, C) :- p(AT, CT), delete(A, C, CT).
% is twice as fast again, but doesn't run backwards with any when declaration.

/*	_2 is list _1 reversed */
reverse(A, B) :- 
	/* $rev2(B, [], A),	/* if B is instantiated */
	$rev2(A, [], B).		/* if A is instantiated (can have both) */

/*	_3 is the reverse of _1 appended to _2 */
:- $rev2(A, _, _) when A.
$rev2([], A, A).
$rev2(A.B, C, D) :-
	$rev2(B, A.C, D).

%	BUG!  Everything below here needs documentation.

:- nth0(N, List, Elem) when ground(N) or List.
nth0(0, X._, X).
nth0(N, _.List, Elem) :-
	N > 0,
	plus(N1, 1, N),
	nth0(N1, List, Elem).

:- nth0(N, List, Elem, Rest) when ground(N) or List or Rest.
nth0(0, X.Rest, X, Rest).
nth0(N, X.List, Elem, X.Rest) :-
	N > 0,
	plus(N1, 1, N),
	nth0(N1, List, Elem, Rest).

nth1(N, List, Elem) :-
	plus(N1, 1, N),
	nth0(N1, List, Elem).

nth1(N, List, Elem, Rest) :-
	plus(N1, 1, N),
	nth0(N1, List, Elem, Rest).

:- mapList(_, List) when List.
mapList(_, []).
mapList(Pred, L0.L) :-
	call(Pred, L0),
	mapList(Pred, L).
	
:- mapList(_, L, M) when L or M.
mapList(_, [], []).
mapList(Pred, L0.L, M0.M) :-
	call(Pred, L0, M0),
	mapList(Pred, L, M).
	
:- mapList(_, L, M, N) when L or M or N.
mapList(_, [], [], []).
mapList(Pred, L0.L, M0.M, N0.N) :-
	call(Pred, L0, M0, N0),
	mapList(Pred, L, M, N).

:- cumList(_, L, _, M) when L or M.
cumList(_, [], _, []).
cumList(Pred, L0.L, V0, M0.M) :-
	call(Pred, L0, V0, M0),
	cumList(Pred, L, M0, M).

:- cumList(_, L, M, _, N) when L or M or N.
cumList(_, [], [], _, []).
cumList(Pred, L0.L, M0.M, V0, N0.N) :-
	call(Pred, L0, M0, V0, N0),
	cumList(Pred, L, M, N0, N).

:- cumList(_, L, M, N, _, O) when L or M or N or O.
cumList(_, [], [], [], _, []).
cumList(Pred, L0.L, M0.M, N0.N, V0, O0.O) :-
	call(Pred, L0, M0, N0, V0, O0),
	cumList(Pred, L, M, N, O0, O).

:- scanList(_, L, _, _) when L.
scanList(_, [], V, V).
scanList(Pred, L0.L, V0, V) :-
	call(Pred, L0, V0, V1),
	scanList(Pred, L, V1, V).

:- scanList(_, L, M, _, _) when L or M.
scanList(_, [], [], V, V).
scanList(Pred, L0.L, M0.M, V0, V) :-
	call(Pred, L0, M0, V0, V1),
	scanList(Pred, L, M, V1, V).

:- scanList(_, L, M, N, _, _) when L or M or N.
scanList(_, [], [], [], V, V).
scanList(Pred, L0.L, M0.M, N0.N, V0, V) :-
	call(Pred, L0, M0, N0, V0, V1),
	scanList(Pred, L, M, N, V1, V).

:- someMember(_, L) when L.
someMember(Pred, L0._) :-
	call(Pred, L0).
someMember(Pred, _.L) :-
	someMember(Pred, L).

:- someMember(_, L, M) when L or M.
someMember(Pred, L0._, M0._) :-
	call(Pred, L0, M0).
someMember(Pred, _.L, _.M) :-
	someMember(Pred, L, M).

:- someMember(_, L, M, N) when L or M or N.
someMember(Pred, L0._, M0._, N0._) :-
	call(Pred, L0, M0, N0).
someMember(Pred, _.L, _.M, _.N) :-
	someMember(Pred, L, M, N).

:- someMemberchk(_, L) when L.
someMemberchk(Pred, L0._) :-
	call(Pred, L0),
	!.
someMemberchk(Pred, _.L) :-
	someMemberchk(Pred, L).

:- someMemberchk(_, L, M) when L or M.
someMemberchk(Pred, L0._, M0._) :-
	call(Pred, L0, M0),
	!.
someMemberchk(Pred, _.L, _.M) :-
	someMemberchk(Pred, L, M).

:- someMemberchk(_, L, M, N) when L or M or N.
someMemberchk(Pred, L0._, M0._, N0._) :-
	call(Pred, L0, M0, N0),
	!.
someMemberchk(Pred, _.L, _.M, _.N) :-
	someMemberchk(Pred, L, M, N).

%	Awefully non-logical
%
%	The when declaration should really be
%		L or (Front = [] -> Back).
:- spanList(_, L, Front, Back) when L or Front and Back.
spanList(Pred, [], [], []).
spanList(Pred, L.Ls, Front, Back) :-
	( cons(Front) ->
		Front = L.Front1,
		call(Pred, L),
		spanList(Pred, Ls, Front1, Back)
	; nonvar(Front) ->		% Front == [], but this is actually more sensible
		Front = [],
		Back = L.Ls,
		\+ call(Pred, L)
	; call(Pred, L) ->
		Front = L.Front1,
		spanList(Pred, Ls, Front1, Back)
	;	Front = [],
		Back = L.Ls
	).

:- groupList(_, L, Seps, Groups) when L.
groupList(Pred, L0, Seps0, Groups0) :-
	( cons(L0) ->
		Groups0 = Front.Groups,
		spanList(Pred, L0, Front, L1),
		( cons(L1) ->
			L1 = Sep.L,
			Seps0 = Sep.Seps,
			groupList(Pred, L, Seps, Groups)
		;	groupList(Pred, L1, Seps0, Groups)
		)
	;	L0 = [],
		Seps0 = [],
		Groups0 = []
	).
