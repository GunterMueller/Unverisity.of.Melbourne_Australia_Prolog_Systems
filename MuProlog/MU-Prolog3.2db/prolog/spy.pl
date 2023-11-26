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


?-hide(flags(2).spypt(1).[]).

spy [].
spy A.B :-
	!,
	spy A,
	spy B.
spy A :-
	$debug(0),
	$spy1(A).

nospy [].
nospy A.B :-
	!,
	nospy A,
	nospy B.
nospy A :-
	$debug(0),
	$nospy1(A).

$spy1(P) :-
	$expand1(P, Q),
	spypt(Q),
	!,
	write(2, 'spypoint on ', 0),
	write(2, P),
	write(2, ' already exists', 0),
	nl,
	fail.
$spy1(P) :-
	$expand1(P, Q),
	$proc(Q, _, _),
	!,
	assert(spypt(Q)),
	asserta((Q :- $tf, !, $tcall(Q))),
	write(2, 'spypoint added to ', 0),
	writeln(2, P).
$spy1(P) :-
	write(2, 'no such procedure as ', 0),
	writeln(2, P),
	fail.

$nospy1(P) :-
	$expand1(P, Q),
	retract(spypt(Q)),
	retract((Q :- $tf, !, $tcall(Q))),
	write(2, 'spypoint removed', 0),
	nl,
	!.
$nospy1(P) :-
	write(2, 'no spypoint on ', 0),
	writeln(2, P),
	fail.

?- retractall($rmspy(_)).

$rmspy(Q) :-
	retract(spypt(Q)),
	retract((Q :- $tf, !, $tcall(Q))),
	!.
$rmspy(_).

debugging :-
	$debug(0),
	write(2, 'There are spypoints on the following procedures', 0),
	nl(2),
	spypt(X),
	functor(X, F, N),
	write(2, F),
	write(2, '(', 0),
	write(2, N),
	write(2, ')', 0),
	nl,
	fail.
debugging.

nodebug :-
	$debug(0),
	retract(spypt(X)),
	retract((X :- $tf, !, $tcall(X))),
	fail.
nodebug :-
	write(2, 'all spypoints removed', 0),
	nl.

$expand1(P, Q) :-
	P =.. P.[],
	$proc(Q, P, _).
$expand1(P, Q) :-
	$expand(P, Q).

spypt(1).
?-retract(spypt(1)).

$flag(F, O, N) :-
	retract(flags(F, O)),
	assert(flags(F, N)),
	!.

flags(tff, 1).
flags(skip, 0).

$tf :-
	$flag(tff, 1, 0),
	!.
$tf :-
	$flag(tff, 0, 1),
	fail.

$tcall(P) :-
	flags(skip, 0),
	repeat,
	write(1, 'CALL	', 0),
	write(1, P),
	write(1, ' ? ', 0),
	$getcmd(C),
	(	C.[] = "f",
		!,
		fail
	;	 
		C.[] = "a",
		abort
	;	 
		C.[] = "t",
		!,
		(	true
		;
			$tfail(P)
		)
	;	 
		C.[] = "s",
		!,
		$skipon,
		P,
		$skipoff
	;	 
		C.[] = "b",
		!,
		break,
		$tfail(P)
	;	 
		C = 10,
		!,
		$tfail(P)
	;
		write(1, 'options are abort,fail,true,skip,break,<CR>', 0),
		nl(1),
		fail
	).
$tcall(P) :-
	flags(skip, 1),
	P.

$tfail(P) :-
	$tprre(P).
$tfail(P) :-
	flags(skip, 0),
	write(1, 'FAIL	', 0),
	writeln(1, P),
	fail.

$tprre(P) :-
	P,
	(	write(1, 'PROVED	', 0),
		writeln(1, P)
	;
		$getvcmd(P, C),
		(	C.[] = "a",
			abort
		;		 
			C.[] = "b",
			!,
			break
		;		 
			C.[] = "f",
			!,
			fail
		;
			C.[] = "t"
		)
	).

$getvcmd(P, C) :-
	repeat,
	write(1, 'REDO	', 0),
	write(1, P),
	write(1, ' ? ', 0),
	$getcmd(C),
	(	C = 10
	;	 
		C.[] = "b"
	;	 
		C.[] = "a"
	;	 
		C.[] = "f"
	;	 
		C.[] = "t"
	;
		write(1, 'options are abort,fail,true,break,<CR>', 0),
		nl,
		fail
	),
	!.

$getcmd(C) :-
	getc(0, C),
	(	C = 10
	;
		skip(10)
	),
	!.

$skipon :-
	$flag(skip, _, 1).
$skipon :-
	$flag(skip, _, 0),
	fail.

$skipoff :-
	$flag(skip, _, 0).
$skipoff :-
	$flag(skip, _, 1),
	fail.

?-$protect((spy 1)),
	$protect((nospy 1)),
	$protect(debugging),
	$protect(nodebug),
	$protect($spy1(1)),
	$protect($nospy1(1)),
	$protect($rmspy(1)),
	$protect($expand1(1, 1)),
	$protect($flag(1, 1, 1)),
	$protect($tf),
	$protect($tcall(1)),
	$protect($tprre(1)),
	$protect($getvcmd(1, 1)),
	$protect($getcmd(1)),
	$protect($skipon),
	$protect($skipoff),
	$protect($tfail(1)).
