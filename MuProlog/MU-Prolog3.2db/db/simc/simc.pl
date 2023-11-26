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

% specify the location of a couple of binaries

%##$simc_newrel('LIB/db/simc/newrel').
$simc_newrel('/usr/lib/prolog/db/simc/newrel').
%##$simc_dbparams('LIB/db/simc/dbparams simc').
$simc_dbparams('/usr/lib/prolog/db/simc/dbparams simc').

?-use_if dynamic_loading.

% load the superimposed coding scheme

%##?-dload(['LIB/dload/simc.o','LIB/db/simc/dblib'],
?-dload(['/usr/lib/prolog/dload/simc.o','/usr/lib/prolog/db/simc/dblib'],
	' ',
	['_p_simc_query',	'_p_simc_next',		'_p_simc_end',
	'_p_simc_assert',	'_p_simc_delete',	'_p_simc_abort'],
	[$simc_query(_,_,_),	$simc_next(_,_),	$simc_end(_),
	$simc_assert(_,_),	$simc_delete(_),	$simc_abort(_)]).

?-use_end.

?-	$unprotect($db_assert(1, 1, 1)),
	$unprotect($db_retract(1, 1, 1)),
	$unprotect($db_retractall(1, 1, 1)),
	$unprotect($db_define(1, 1, 1, 1)),
	$unprotect($db_undefine(1, 1, 1)),
	$unprotect($db_setup(1, 1, 1)).

?-assert(($db_assert(D, R, simc) :-
	$simc_assert(D, R))).

?-assert(($db_retract(D, R, simc) :-
	$simc_retract(D, R))).

?-assert(($db_retractall(D, R, simc) :-
	$simc_rall(D, R))).

?-assert(($db_define(D, R, N, simc(Args)) :-
	$simc_define(D, R, N, Args))).

?-assert(($db_undefine(DS, R, simc) :-
	$simc_undefine(DS, R))).

?-assert(($db_setup(D, R, simc):-
	assert((R :- $simc_retrieve(D, R))))).

$simc_retrieve(D, P) :-		% get answers to a query from external db
	$db_onabort($simc_abort(F)),
	$simc_query(D, P, F),
%	$simc_sfbquery(D, P, "********************************", F),
	repeat,
	$simc_next(F, Q),
	(	Q = (?-E),
		(	E = db_end,
			$simc_end(F),
			!,
			fail
%		;		 
%			E = db_end(N),
%			(	simc_count(N)
%			;
%				true
%			),
%			$simc_end(F),
%			!,
%			fail
		;		 
			E = db_error(N),
			write(N),
			$simc_emess(N, M),
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

$simc_retract(D, P) :-		% delete matching records one by one from db
	$db_onabort($simc_abort(F)),
	$simc_query(D, P, F),
	repeat,
	$simc_next(F, Q),
	(	Q = (?-E),
		(	E = db_end,
			$simc_end(F),
			!,
			fail
		;		 
			E = db_error(N),
			write(N),
			$simc_emess(N, M),
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
		P = Q,
		$simc_delete(F)
	).

$simc_rall(D, P) :-	% delete all matching records from database
	$db_onabort($simc_abort(F)),
	$simc_query(D, P, F),
	repeat,
	$simc_next(F, Q),
	(	Q = (?-E),
		(	E = db_end,
			$simc_end(F),
			!,
			true
		;		 
			E = db_error(N),
			write(N),
			$simc_emess(N, M),
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
		P = Q,
		$simc_delete(F),
		fail
	).
$simc_rall(D, P).



%simc_count(0).				% for taking stats
%?-$ret((simc_count(0) :- true)).

$simc_emess(exec, 'mudd: cannot execute') :-
	!.
$simc_emess(open, 'cannot open for reading') :-
	!.
$simc_emess(nkeys, 'wrong number of keys in database') :-
	!.
$simc_emess(X, 'Unknown database error').


% $simc_define
%
% call:
% newrel DS Rname simc `dbparams Scheme Av Arity Nrecs SegSz Nd` Skel)

$simc_define(DS, Rname, Arity, [Av,Nrecs,SegSz,Nd,Skel]) :-
	$argstos([Av,Arity,Nrecs,SegSz,Nd], Dbpargs),
	name(Rname, Rs),
	$simc_newrel(NR),
	$simc_dbparams(DBP),
	name(NR, NRS),
	name(DBP, DBPS),
	$app(NRS, 32.DS, S),
	$app(S, 32.Rs, S1),
	$app(S1, " simc `", S2),
	$app(S2, DBPS, S3),
	$app(S3, 32.Dbpargs, S4),
	$app(S4, "` ", S5),
	$app(S5, 34.Skel, S6),
	$app(S6, 34.[], Cmd),
	printf("%s%c",[Cmd,10]),
	shell(Cmd).


$simc_undefine(DS, R) :-		% remove relation
	$app("rm -rf ", DS, S1),	% rm -rf dbname/relname
	Sl is "/",
	name(R, RS),
	$app(S1, Sl.RS, Cmd),
	printf("%s",[Cmd,10]),
	shell(Cmd).

?- protect([
	$db_assert(3),
	$db_retract(3),
	$db_retractall(3),
	$db_define(4),
	$db_undefine(3),
	$db_setup(3),
	$simc_retrieve(2),
	$simc_emess(2),
	$simc_define(4),
	$simc_undefine(2),
	$simc_newrel(1),
	$simc_dbparams(1)]).
?-hidden.
