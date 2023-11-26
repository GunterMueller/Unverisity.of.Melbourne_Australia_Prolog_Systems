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


?-$hide(ondisc(1, 1)), hide(dbtmp(1)).

ondisc(0, 0).
?-retract(ondisc(0, 0)).

			% modify db access preds to check for external db

?-$nassert((assert((X :- Y)) :-
	ondisc(D, X), writeln('cant assert rules in database'), !, fail), 1).
?-$nassert((assert(X) :-
	ondisc(D, X), !, $dbassert(D, X)), 3).

?-$nassert((asserta((X :- Y)) :-
	ondisc(D, X), writeln('cant assert rules in database'), !, fail), 1).
?-$nassert((asserta(X) :-
	ondisc(D, X), !, $dbassert(D, X)), 3).

?-$nassert((retract((A :- true)) :-
	ondisc(D, A), !, $dbretract(D, A)), 1).
?-$nassert((retract(A) :-
	ondisc(D, A), !, $dbretract(D, A)), 3).

?-$nassert((retractall(A) :-
	ondisc(D, A), !, $dbretractall(D, A)), 1).

?-asserta((clause(A, true) :-
	ondisc(D, A), !, A)).

?-assert(($emess(200) :-
	write(2, 'error in accessing external database'))).

dbfact(D, P) :-			% get answers to a query from external db
	$db_onabort($rlhp0_abort(F)),
	$dbquery(D, P, F),
	repeat,
	read(F, Q),
	(	Q = (?-E),
		(	E = db_end,
			$db_end(F),
			!,
			fail
%		;		 
%			E = db_end(N),
%			(	dbcount(N)
%			;
%				true
%			),
%			$db_end(F),
%			!,
%			fail
		;		 
			E = db_error(N),
			write(N),
			$db_emess(N, M),
			write(' : '),
			writeln(M),
			fail
		;
			E = end,
			writeln('db_end expected'),
			!,
			fail
		)
	;
		P = Q
	).

%dbcount(0).				% for taking stats
%?-$ret((dbcount(0) :- true)).

$db_emess(exec, 'mudd: cannot execute') :-
	!.
$db_emess(open, 'cannot open for reading') :-
	!.
$db_emess(nkeys, 'wrong number of keys in database') :-
	!.
$db_emess(X, 'Unknown database error').

dbcons(D) :-				% (re)consult external database
	name(D, DS),
	$app(DS, "/.con", FS),
	name(F, FS),
	$dbcon1(D, F),
	$app(DS, "/.rules", RS),
	name(R, RS),
	reconsult(R).

$dbcon1(D, F) :-
	open(F, N, r),
	repeat,
	read(N, T),
	$dbdo(D, T),
	close(N),
	!.

$dbdo(D, (?-end)).
$dbdo(D, relation(R)) :-
	retractall(ondisc(D, R)),
	retractall(R),
	assert((R :- dbfact(D, R))),
	assert(ondisc(D, R)),
	fail.

dbcreate(D) :-		% create new database (directory + .rules + .con)
	name(D, DS),
	$app("mkdir ", DS, C1),
	shell(C1),
	$app("touch ", DS, C2),
	$app(C2, "/.rules", C3),
	shell(C3),
	$app(C2, "/.con", C4),
	shell(C4).

dbrules(D, R) :-		% copy rules file into external db
	name(D, DS),
	name(R, RS),
	$app("cp ", RS, C1),
	$app(C1, 32.DS, C2),
	$app(C2, "/.rules", C3),
	shell(C3).

removerel(D, R) :-		% remove relation
	retractall(dbtmp(_)),
	name(D, DS),
	$app(DS, "/.con", CS),
	name(C, CS),
	open(C, F, r),
	repeat,			% read current db contents into dbtmp relation
	read(F, T),
	(
		eof(T)
	;
		assert(dbtmp(T)),
		fail
	),
	!,
	close(F),
	dbtmp(relation(R1)),
	functor(R1, R, _),
	retract(dbtmp(relation(R1))),	% remove relation from dbtmp
	!,
	retract(ondisc(D, R1)),		% in case we have done a dbcons
	retractall(R1),
	$app("rm ", DS, S1),		% rm dbname/relname dbname/.relname
	Sl is "/",
	name(R, RS),
	$app(S1, Sl.RS, S2),
	$app(S2, 32.DS, S3),
	Sd is ".",
	$app(S3, Sl.Sd.RS, S4),
	shell(S4),
	$write_con(C).

		% write back updated list of relations to .con file
$write_con(C) :-
	open(C, F, w),
	retract(dbtmp(R)),
	write(F, R),
	write(F, '.'),
	nl(F),
	fail.
$write_con(_).

			% create new relation
createrel(D, [Rname, Nkeys, Lc, Bsize| Rest]) :-
	functor(R, Rname, Nkeys),
	retractall(R),
	name(D, DS),
	$app(DS, "/.con", CS),
	name(C, CS),
	not $exists(C, Rname),		% reln should not exist already
	open(C, N1, a),			% append new reln to db contents
	write(N1, relation(R)),
	write(N1, '.'),
	nl(N1),
	close(N1),
	$argstos(Nkeys.Lc.Bsize.Rest, Argss),	% convert args to a string
	name(Rname, Rs),
	$dbnewrel(NR),
	name(NR, NRS),
	$app(NRS, 32.DS, S),
	$app(S, 32.Rs, S1),
	$app(S1, 32.Argss, Cmd),
	shell(Cmd),			% call newrel with arg string
	assert((R :- dbfact(D, R))),	% in case we did dbcons
	assert(ondisc(D, R)).

$exists(C, R) :-		% does R already exist in .con file C ?
	open(C, F, r),
	repeat,
	read(F, T),
	(
		eof(T)
	;
		T = relation(P),
		functor(P, R, _)
	),
	!,
	close(F),
	T = relation(P),
	functor(P, R, _),
	write(2, 'A relation of that name already exists'),
	nl(2).

$argstos([], []).
$argstos(I.S, A) :-
	$argstos(S, B),
	$addnum(I, 32.B, A).

$addnum(I, S, D.S) :-
		I < 10,
		D is I + "0".
$addnum(I, S, S1) :-
		I >= 10,
		D is I mod 10 + "0",
		I2 is I/10,
		$addnum(I2, D.S, S1).

$db_onabort(_).

?- $protect(dbfact(1, 1)),
	$protect(dbcons(1)),
	$protect($db_emess(1, 1)),
	$protect($dbcon1(1, 1)),
	$protect($dbdo(1, 1)),
	$protect(dbcreate(1)),
	$protect(dbrules(1, 1)),
	$protect(createrel(1, 1)),
	$protect(removerel(1, 1)),
	$protect($write_con(1)),
	$protect($exists(1, 1)),
	$protect($argstos(1, 1)),
	$protect($db_onabort(1)),
	$protect($addnum(1, 1, 1)).
