/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

/* Actually this was written by someone in Edinburgh, I think */

$gexpand(P0, Q0, P, Q) :-
	$dcglhs(P0, S0, S, P),
	$dcgrhs(Q0, S0, S, Q1),
	$flatconj(Q1, Q).

$dcglhs((NT, Ts), S0, S, P) :-
	!,
	nonvar(NT),
	$islist(Ts),
	$tag(NT, S0, S1, P),
	$app(Ts, S0, S1).
$dcglhs(NT, S0, S, P) :-
	nonvar(NT),
	$tag(NT, S0, S, P).

$dcgrhs((X1, X2), S0, S, P) :-
	!,
	$dcgrhs(X1, S0, S1, P1),
	$dcgrhs(X2, S1, S, P2),
	$and(P1, P2, P).
$dcgrhs((X1 ; X2), S0, S, (P1 ; P2)) :-
	!,
	$dcgor(X1, S0, S, P1),
	$dcgor(X2, S0, S, P2).
$dcgrhs({P}, S, S, P) :-
	!.
$dcgrhs(!, S, S, !) :-
	!.
$dcgrhs(Ts, S0, S, true) :-
	$islist(Ts),
	!,
	$app(Ts, S, S0).
$dcgrhs(X, S0, S, P) :-
	$tag(X, S0, S, P).

$dcgor(X, S0, S, P) :-
	$dcgrhs(X, S0a, S, Pa),
	(	var(S0a),
		S0a \== S,
		!,
		S0 = S0a,
		P = Pa
	;
		P = (S0 = S0a, Pa)
	).

$tag(X, S0, S, P) :-
	X =.. F.A,
	$app(A, S0.S.[], AX),
	P =.. F.AX.

$and(true, P, P) :-
	!.
$and(P, true, P) :-
	!.
$and(P, Q, (P, Q)).

$flatconj(A, A) :-
	var(A),
	!.
$flatconj((A, B), C) :-
	!,
	$fc1(A, C, R),
	$flatconj(B, R).
$flatconj(A, A).

$fc1(A, (A, R), R) :-
	var(A),
	!.
$fc1((A, B), C, R) :-
	!,
	$fc1(A, C, R1),
	$fc1(B, R1, R).
$fc1(A, (A, R), R).

$islist([]) :-
	!.
$islist(_._).

?-$protect($gexpand(1, 1, 1, 1)),
	$protect($dcglhs(1, 1, 1, 1)),
	$protect($dcgrhs(1, 1, 1, 1)),
	$protect($tag(1, 1, 1, 1)),
	$protect($and(1, 1, 1)),
	$protect($flatconj(1, 1)),
	$protect($fc1(1, 1, 1)),
	$protect($islist(1)),
	$protect($dcgor(1, 1, 1, 1)).
