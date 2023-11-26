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

/* Adapted from some code written by Lawrence Byrd at Edinburgh */

(listing) :-
	%$debug(0),
	$proc(Pred, _, _),
	$list_clauses(Pred).
(listing).

listing(V) :-
	$debug(0),
	var(V),
	!,
	write(2, 'Cant list a variable.', 0),
	nl.
listing([]) :-
	!.
listing(X.Rest) :-
	$debug(0),
	!,
	listing(X),
	listing(Rest).
listing(X) :-
	$functorspec(X, Name, Arity),
	$proc(Pred, Name, Arity),
	$list_clauses(Pred).
listing(_).

$functorspec(X, X, _) :-
	atom(X).
$functorspec(X, F, N) :-
	X =.. F.N.[].

$list_clauses(Pred) :-
	nl,
	$wlist(Pred),
	$nclause((Pred :- _), C, _),	/* "clause" would list external dbs */
	(Pred :- Body) = C,
	$write_clause(Pred, Body),
	put("."),
	nl,
	fail.

$write_clause(Head, Body) :-
	writef(Head, 2'110110),
	(	Body == true
	;
		put(" "),
		writef((:-), 0),
		$write_body(Body, 1, ',')
	),
	!.

$write_body(V, I, T) :-
	var(V),
	!,
	$beforelit(T, I),
	writef(V, 2'110110).
$write_body((P, Q), IO, T) :-
	!,
	$write_body(P, IO, T),
	put(","),
	$aftercomma(T, IO, I),
	$write_body(Q, I, ',').
$write_body(if(X), I, T) :-
	var(X),
	!,
	$beforelit(T, I),
	writef('(if ', 0),
	writef(X, 2'110110),
	writef(')', 0).
$write_body((if C then A), I, T) :-
	!,
	nl,
	$tabs(I),
	writef('(if ', 0),
	writef(C, 2'110110),
	writef(' then', 0),
	nl,
	$write_body(A, I, then),
	nl,
	$tabs(I),
	put(")").
$write_body((if X else Y), I, T) :-
	var(X),
	!,
	$beforelit(T, I),
	writef('(if ', 0),
	writef((X else Y), 2'110110),
	writef(')', 0).
$write_body((if C then A else B), I, T) :-
	!,
	nl,
	$tabs(I),
	writef('(if ', 0),
	writef(C, 2'110110),
	writef(' then', 0),
	nl,
	$write_body(A, I, then),
	nl,
	$tabs(I),
	writef(else, 0),
	nl,
	$write_body(B, I, else),
	nl,
	$tabs(I),
	put(")").
$write_body((CA ; B), I, T) :-
	nonvar(CA),
	(C -> A) = CA,
	!,
	nl,
	$tabs(I),
	writef('( ', 0),
	writef(C, 2'110110),
	writef(' ->', 0),
	nl,
	$write_body(A, I, then),
	nl,
	$tabs(I),
	writeln((;)),
	$write_body(B, I, else),
	nl,
	$tabs(I),
	put(")").
$write_body((C -> A), I, T) :-
	!,
	nl,
	$tabs(I),
	writef('( ', 0),
	writef(C, 2'110110),
	writef(' ->', 0),
	nl,
	$write_body(A, I, then),
	nl,
	$tabs(I),
	put(")").
$write_body((P ; Q), I, T) :-
	!,
	(	T = ;,
		$tabs(I),
		put(" "),
		$write_body(P, I, ;)
	;
		nl,
		$tabs(I),
		put("("),
		$write_body(P, I, '(')
	),
	nl,
	$tabs(I),
	put(";"),
	$write_body(Q, I, ;),
	(	T = ;
	;
		nl,
		$tabs(I),
		put(")")
	),
	!.
$write_body(X, I, T) :-
	$beforelit(T, I),
	writef(X, 2'110110).

$aftercomma(',', I, I) :-
	!.
$aftercomma(_, IO, I) :-
	I is IO + 1.

$beforelit('(', _) :-
	!,
	put("	").
$beforelit((;), I) :-
	!,
	nl,
	$tabs(I+1).
$beforelit((then), I) :-
	!,
	$tabs(I+1).
$beforelit((else), I) :-
	!,
	$tabs(I+1).
$beforelit(_, I) :-
	nl,
	$tabs(I).

$tabs(0) :-
	!.
$tabs(N) :-
	put(9),
	N1 is N - 1,
	$tabs(N1).

portraycl((A :- B)) :-
	!,
	$write_clause(A, B).
portraycl(A) :-
	$write_clause(A, true).

?-$protect($functorspec(1, 1, 1)),
	$protect($write_body(1, 1, 1)),
	$protect($list_clauses(1)),
	$protect($write_clause(1, 1)),
	$protect($aftercomma(1, 1, 1)),
	$protect($beforelit(1, 1)),
	$protect($tabs(1)),
	$protect(listing),
	$protect(listing(1)),
	$protect(portraycl(1)).
