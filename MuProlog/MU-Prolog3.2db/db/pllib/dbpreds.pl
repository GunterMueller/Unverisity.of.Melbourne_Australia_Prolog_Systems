/************************************************************************
*									*
*			MU-PROLOG DATABASE				*
*			========= ========				*
*									*
* (C) Copyright 1985 Lee Naish, James Thom, Melbourne University	*
*									*
*	Written by Lee Naish & James Thom				*
*	Department of Computer Science,					*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/


?-$hide(ondisc(1, 1, 1)), hide($db_tmp(1)), hide($db_loaded(1)).

ondisc(0, 0, 0).
?-retract(ondisc(0, 0, 0)).

$db_loaded(0).
?-retract($db_loaded(0)).

$db_tmp(0).
?-retract($db_tmp(0)).

$db_onabort(_).

			% modify db access preds to check for external db

?-$unprotect(assert(1)),
	$unprotect(asserta(1)),
	$unprotect(assertz(1)),
	$unprotect(retract(1)),
	$unprotect(retractall(1)),
	$unprotect(clause(1,1)),
	$unprotect($emess(1)).

?-retractall(assert(_)).
?-retractall(asserta(_)).
?-retractall(assertz(_)).
?-retractall(retract(_)).
?-retractall(clause(_,_)).

%
% redefine assert
%

?-$nassert((assert((X :- Y)) :-
	ondisc(D, X, S),
	writeln('cant assert rules in external database'),
	!, fail), _).
?-$nassert((assert((X:-Y)) :-
	!, $nassert((X:-Y),N)),_).
?-$nassert((assert(X) :-
	ondisc(D, X, S),
	!, $db_assert(D, X, S)), _).
?-$nassert((assert(X) :-
	$nassert((X:-true),N)),_).

%
% redefine asserta & assertz
% should not be allowed for external db preds
%

asserta(X) :-
	ondisc(D, X, S),
	writeln('cant use asserta in external database'),
	!,
	fail.
asserta((X :- Y)) :-
	!,
	$nassert((X :- Y), 1).
asserta(X) :-
	$nassert((X :- true), 1).

assertz(X) :-
	ondisc(D, X, S),
	writeln('cant use assertz in external database'),
	!,
	fail.
assertz(X) :-
	assert(X).
%
% redefine retract
%

retract((X :- true)) :-
	ondisc(D, X, S), !, $db_retract(D, X, S).
retract((A :- B)) :-
	!,
	$ret((A :- B)).
retract(X) :-
	ondisc(D, X, S), !, $db_retract(D, X, S).
retract(A) :-
	$ret((A :- true)).

%
% redefine retractall by adding extra clause
%

?-$nassert((retractall(X) :-
	ondisc(D, X, S), !, $db_retractall(D, X, S)), 1).
%
% remaining clauses in retractall unaltered
%
%	retractall(A) :-
%		$ret((A :- B)),
%		fail.
%	retractall(A).

%
% redefine clause
%

clause(X, true) :-
	ondisc(D, X, S), !, X.
clause(A, B) :-
	$nclause((A :- B), C, N),
	(A :- B) = C.

%
% add extra emess procedure
%

?-assert(($emess(200) :-
	write(2, 'error in accessing external database'))).

%
% reprotect all the systems just altered
%

?-$protect(assert(1)),
	$protect(asserta(1)),
	$protect(assertz(1)),
	$protect(retract(1)),
	$protect(retractall(1)),
	$protect(clause(1,1)),
	$protect($emess(1)).


db_cons(D) :-				% (re)consult external database
	name(D, DS),
	$app(DS, "/.con", FS),
	name(F, FS),
	$db_con1(D, F),
	$app(DS, "/.rules", RS),
	name(R, RS),
	reconsult(R).

$db_con1(D, F) :-
	open(F, N, r),
	repeat,
	read(N, T),
	$db_do(D, T),
	close(N),
	!.

$db_do(D, (?-end)).
$db_do(D, relation(R, S)) :-
	retractall(ondisc(_, R, _)),
	retractall(R),
	$db_load(S),
	$db_setup(D, R, S),
	assert(ondisc(D, R, S)),
	fail.

$db_load(S) :- ~$db_loaded(S), !,
	lib(S),
	assert($db_loaded(S)).
$db_load(S).

db_create(D) :-		% create new database (directory + .rules + .con)
	name(D, DS),
	$app("mkdir ", DS, C1),
	shell(C1),
	$app("touch ", DS, C2),
	$app(C2, "/.rules", C3),
	shell(C3),
	$app(C2, "/.con", C4),
	shell(C4).

db_rules(D, R) :-		% copy rules file into external db
	name(D, DS),
	name(R, RS),
	$app("cp ", RS, C1),
	$app(C1, 32.DS, C2),
	$app(C2, "/.rules", C3),
	shell(C3).

db_define(D, Rname, N, Scheme) :-			 % create new relation
					% check args and convert as needed
	name(D, DS),
	functor(R, Rname, N),
	Scheme =.. S.Args,
					% retract any existing defn of R
	retractall(R),
					% load Scheme if not already loaded
	$db_load(S),
					% define D, R, S
	$db_define(DS, Rname, N, Scheme),
	$db_setup(D, R, S),
	assert(ondisc(D, R, S)),
					% update .con file
	$app(DS, "/.con", CS),
	name(C, CS),
	not $db_exists(C, Rname),		% reln should not exist already
	open(C, N1, a),				% append new reln to db contents
	write(N1, relation(R,S)),
	write(N1, '.'),
	nl(N1),
	close(N1).

db_undefine(D, R) :-		% remove relation
	retractall($db_tmp(_)),
	name(D, DS),
	$app(DS, "/.con", CS),
	name(C, CS),
	open(C, F, r),
	repeat,			% read current db contents into $db_tmp relation
	read(F, T),
	(
		eof(T)
	;
writeln(assert($db_tmp(T))),
		assert($db_tmp(T)),
		fail
	),
	!,
	close(F),
	$db_tmp(relation(R1,S)),
	functor(R1, R, _),
	retract($db_tmp(relation(R1, S))),	% remove relation from $db_tmp
	!,
	retract(ondisc(D, R1, S)),		% in case we have done a dbcons
	retractall(R1),
	$db_undefine(DS, R, S),
	$db_write_con(C).

$db_write_con(C) :-	% write back updated list of relations to .con file
	open(C, F, w),
	retract($db_tmp(R)),
	write(F, R),
	write(F, '.'),
	nl(F),
	fail.
$db_write_con(_).

$db_exists(C, R) :-		% does R already exist in .con file C ?
	open(C, F, r),
	repeat,
	read(F, T),
	(
		eof(T)
	;
		T = relation(P, _),
		functor(P, R, _)
	),
	!,
	close(F),
	T = relation(P, _),
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
?- $protect(db_retrieve(1, 1)),
	$protect(db_cons(1)),
	$protect($db_con1(1, 1)),
	$protect($db_do(1, 1)),
	$protect($db_load(1)),
	$protect(db_create(1)),
	$protect(db_rules(1, 1)),
	$protect(db_define(1, 1, 1, 1)),
	$protect(db_undefine(1, 1)),
	$protect($db_onabort(1)),
	$protect($db_write_con(1)),
	$protect($argstos(1, 1)),
	$protect($addnum(1, 1, 1)),
	$protect($db_exists(1, 1)).

?-hidden.
